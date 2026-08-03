module Ide.Plugin.Export.Exports
  ( isExplicit
  , unsafeToRefine
  , ExportList
  , exportListOf
  , addExport
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  , retainExports
  , addExportList
  , isReferencedExternally
  ) where

import           Control.Monad.Extra
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Utf16.Rope.Mixed      (Rope)
import           Development.IDE.Core.Text
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Shake
import           HieDb
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Utils
import           Language.Haskell.GHC.ExactPrint
import           Language.LSP.Protocol.Types

isExplicit :: ParsedSource -> Bool
isExplicit = isJust . hsmodExports . unLoc

anyExportItem :: (IE GhcPs -> Bool) -> ParsedSource -> Bool
anyExportItem p ps = case hsmodExports (unLoc ps) of
  Nothing          -> False
  Just (L _ items) -> any (p . unLoc) items

hasModuleReexport :: ParsedSource -> Bool
hasModuleReexport = anyExportItem isModuleContents
  where
    isModuleContents IEModuleContents{} = True
    isModuleContents _                  = False

exportListSpan :: ParsedSource -> Maybe Range
exportListSpan ps = hsmodExports (unLoc ps) >>= srcSpanToRange . getLoc

-- | Whether trimming the export list would lose information.
unsafeToRefine :: ModSummary -> Maybe Rope -> ParsedSource -> Bool
unsafeToRefine summ msrc ps =
  hasModuleReexport ps
    -- A missing buffer makes a CPP export list read as directive-free.
    || (isCpp summ && isNothing msrc)
    || maybe False (.hasCpp) (exportListOf msrc ps)

-- | Whether the export list already brings @n@ into scope, either as an entry
-- head or as a child of a bundled entry such as @T(P)@.
isExported :: RdrName -> LExportList -> Bool
isExported n (L _ items) = any (covers . unLoc) items
  where
    nFS = rdrNameFS n
    covers ie =
      parentNameIs nFS ie
        || maybe False (any ((== nFS) . lieWrappedNameFS) . (.children)) (ieThingWithParts ie)

{- Note [Reprinting erases CPP directives]

CPP runs before the parser, so reprinting the list through ghc-exactprint erases
directives. The plugin works around that in two ways:
  - An addition splices the new item in after the opening parenthesis.
  - 'reprintExportList' declines every other edit.
-}

data ExportList = ExportList
  { span   :: Range
  , raw    :: LExportList
  , hasCpp :: Bool
    -- | The delta'd list a reprint works from. Lazy, so an edit that splices
    -- instead of reprinting never pays for the walk.
  , delta  :: LExportList
  }

-- | 'Nothing' when the module header has no export list.
exportListOf :: Maybe Rope -> ParsedSource -> Maybe ExportList
exportListOf msrc ps = do
  raw <- hsmodExports (unLoc ps)
  full <- exportListSpan ps
  Just ExportList
    { span = full
    , raw = raw
    , hasCpp = maybe False (spanHasCpp full) msrc
    , delta = makeDeltaAst raw
    }

-- | Pick an edit strategy: a list holding a directive goes to @onCpp@,
-- anything else to a full reprint.
--
-- See Note [Reprinting erases CPP directives].
withExportList
  :: ExportList
  -> (LExportList -> Maybe LExportList)          -- ^ reprint transform
  -> (Range -> LExportList -> Maybe [TextEdit])  -- ^ list holds a directive
  -> Maybe [TextEdit]
withExportList el reprint onCpp
  | el.hasCpp = onCpp el.span el.raw
  | otherwise = do
      newList <- reprint el.delta
      Just [TextEdit el.span (printExportList newList)]

-- | Rewrite the whole export list from the transformed AST.
reprintExportList :: ExportList -> (LExportList -> Maybe LExportList) -> Maybe [TextEdit]
reprintExportList el reprint = withExportList el reprint (\_ _ -> Nothing)

-- | Append @item@ to the export list. 'Nothing' when the list already brings
-- the same name into scope.
addExport :: ExportList -> LIE GhcPs -> Maybe [TextEdit]
addExport el item
  | maybe False (`isExported` el.raw) (ieParentName (unLoc item)) = Nothing
  | otherwise =
      withExportList el (Just . appendIE item) $ \full _ ->
        Just [insertAfterOpen full (printIE item)]

addConstructorExport :: ExportList -> RdrName -> RdrName -> Maybe [TextEdit]
addConstructorExport el parent ctor =
  withExportList el (addCtorUnderParent parent ctor) $ \full exports ->
    (\txt -> [insertAfterOpen full txt]) <$> freshCtorEntry parent ctor (unLoc exports)

-- | Drop the export entries whose head name is absent from @keep@.
retainExports :: ExportList -> [FastString] -> Maybe [TextEdit]
retainExports el keep
  | not (any (dropped . unLoc) items) = Just []
  | otherwise = reprintExportList el (removeAllMatchingIE dropped)
  where
    L _ items = el.raw
    dropped = maybe False ((`notElem` keep) . rdrNameFS) . ieParentName

-- | Splice a fresh @( item, ... )@ list in directly after the module header.
addExportList :: ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
addExportList ps items = do
  Range _ end <- anchor
  Just [TextEdit (Range end end) (" " <> renderExportList items)]
  where
    modl = unLoc ps
    -- The grammar is @'module' modid maybemodwarning maybeexports 'where'@, so
    -- a @{-# DEPRECATED #-}@ pragma takes the list's place after the name.
    anchor = case hsmodDeprecMessage (hsmodExt modl) of
      Just lwarn -> srcSpanToRange (getLoc lwarn)
      Nothing    -> srcSpanToRange . getLoc =<< hsmodName modl

renderExportList :: [LIE GhcPs] -> Text
renderExportList items = "(" <> T.intercalate ", " (map printIE items) <> ")"

isReferencedExternally :: WithHieDb -> [FilePath] -> AvailInfo -> IO Bool
isReferencedExternally withDb exclude avail = anyM referenced (availNames avail)
  where
    referenced n = case nameModule_maybe n of
      Nothing  -> pure False
      Just mod -> do
        rows <- withDb $ \db ->
          findReferences db True (nameOccName n) (Just (moduleName mod)) (Just (moduleUnit mod)) exclude
        pure (any (external mod) rows)
    -- See Note [Generated references]. We need to ignore GHC inserted usages.
    external mod row@(refRow :. _) =
      not (refIsGenerated refRow)
        && not (definedBy mod row)
    definedBy mod (_ :. info) =
      modInfoName info == moduleName mod && modInfoUnit info == moduleUnit mod

-- | Splice @itemTxt@ in right after the opening paren with a trailing comma,
-- @( <itemTxt>, <existing> )@.
insertAfterOpen :: Range -> Text -> TextEdit
insertAfterOpen (Range (Position sl sc) _) itemTxt =
  TextEdit (Range pos pos) (" " <> itemTxt <> ",")
  where
    -- `sc` is the column of `(`, so insert just past it.
    pos = Position sl (sc + 1)

removeExport :: ExportList -> RdrName -> Maybe [TextEdit]
removeExport el name = reprintExportList el (removeMatchingIE matches)
  where
    matches = parentNameIs (rdrNameFS name)

removeConstructorExport :: ExportList -> RdrName -> RdrName -> Maybe [TextEdit]
removeConstructorExport el parent ctor =
  reprintExportList el (removeCtorUnderParent parent ctor)
