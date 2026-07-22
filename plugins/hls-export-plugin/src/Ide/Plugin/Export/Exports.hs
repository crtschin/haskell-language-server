module Ide.Plugin.Export.Exports
  ( isExplicit
  , isExported
  , hasModuleReexport
  , exportListHasCpp
  , addExport
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  , setExportList
  , isReferencedExternally
  ) where

import           Control.Monad.Extra                (anyM)
import           Data.Maybe                         (isJust)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Utf16.Rope.Mixed         (Rope)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error          (srcSpanToRange)
import           Development.IDE.GHC.ExactPrint.CPP (spanHasCpp)
import           Development.IDE.Types.Shake        (WithHieDb)
import           HieDb                              (findReferences)
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Utils
import           Language.Haskell.GHC.ExactPrint    (makeDeltaAst)
import           Language.LSP.Protocol.Types

isExplicit :: ParsedSource -> Bool
isExplicit = isJust . hsmodExports . unLoc

hasModuleReexport :: ParsedSource -> Bool
hasModuleReexport ps = case hsmodExports (unLoc ps) of
  Just (L _ items) -> any (isModuleContents . unLoc) items
  Nothing          -> False
  where
    isModuleContents IEModuleContents{} = True
    isModuleContents _                  = False

-- | The source range spanned by the module's export list, if it has one.
exportListSpan :: ParsedSource -> Maybe Range
exportListSpan ps = hsmodExports (unLoc ps) >>= srcSpanToRange . getLoc

-- | Does the export list's span contain a CPP directive?
exportListHasCpp :: Maybe Rope -> ParsedSource -> Bool
exportListHasCpp msrc ps = maybe False (spanHasCpp msrc) (exportListSpan ps)

-- | Also matches names appearing only as constructor children of an 'IEThingWith' parent.
isExported :: RdrName -> ParsedSource -> Bool
isExported n ps = case hsmodExports (unLoc ps) of
  Nothing          -> False
  Just (L _ items) -> any (covers . unLoc) items
  where
    nFS = rdrNameFS n
    covers ie = parentNameIs nFS ie || isInIE nFS ie

{- Note [Reprinting erases CPP directives]

CPP directives are processed before the parser, so an export list reprinted
through ghc-exactprint silently drops it. The export plugin works around this:
  - Additions splice the new item in surgically after the opening parenthesis.
  - Removals and full replacements actions are omitted.
-}

-- | Extract the export list and pick an edit strategy. Under CPP it splices
-- surgically, otherwise it reprints the whole transformed list.
-- See Note [Reprinting erases CPP directives].
withExportList
  :: Maybe Rope
  -> ParsedSource
  -> (LExportList -> Maybe LExportList)          -- ^ reprint transform
  -> (Range -> LExportList -> Maybe [TextEdit])  -- ^ list holds a directive
  -> Maybe [TextEdit]
withExportList msrc ps reprint onCpp = do
  exports <- hsmodExports (unLoc ps)
  full <- exportListSpan ps
  if spanHasCpp msrc full
    then onCpp full exports
    else do
      newList <- reprint (makeDeltaAst exports)
      Just [TextEdit full (printExportList newList)]

addExport :: Maybe Rope -> ParsedSource -> LIE GhcPs -> Maybe [TextEdit]
addExport msrc ps item =
  withExportList msrc ps (Just . appendIE item) $ \full _ ->
    Just [insertAfterOpen full (printIE item)]

addConstructorExport :: Maybe Rope -> RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
addConstructorExport msrc parent ctor ps =
  withExportList msrc ps (addCtorUnderParent parent ctor) $ \full exports ->
    (\txt -> [insertAfterOpen full txt]) <$> freshCtorEntry parent ctor (unLoc exports)

-- | Replace the module's export list with exactly @desired@, regenerated as a
-- single-line list. See Note [Reprinting erases CPP directives].
setExportList :: Maybe Rope -> ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
setExportList msrc ps desired = case hsmodExports (unLoc ps) of
  Nothing -> addExportList ps desired
  Just _  -> do
    full <- exportListSpan ps
    if spanHasCpp msrc full
      then Nothing
      else Just [TextEdit full (renderExportList desired)]

-- | Splice a fresh @( item, ... )@ list in right after the module name.
addExportList :: ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
addExportList ps items = do
  lmodName <- hsmodName (unLoc ps)
  Range _ end <- srcSpanToRange (getLoc lmodName)
  Just [TextEdit (Range end end) (" " <> renderExportList items)]

-- | Render export items as a parenthesized, comma-separated list.
renderExportList :: [LIE GhcPs] -> Text
renderExportList items = "(" <> T.intercalate ", " (map printIE items) <> ")"

-- | Keep an export if any name it brings into scope is referenced from another
-- module. External code may use a @T(..)@ child, without referencing @T@
-- itself, so we need to check all members.
isReferencedExternally :: WithHieDb -> [FilePath] -> AvailInfo -> IO Bool
isReferencedExternally withDb exclude avail = anyM referenced (availNames avail)
  where
    referenced n = case nameModule_maybe n of
      Nothing  -> pure False
      Just mod -> do
        rows <- withDb $ \db ->
          findReferences db True (nameOccName n) (Just (moduleName mod)) (Just (moduleUnit mod)) exclude
        pure (not (null rows))

-- | Splice @itemTxt@ in right after the opening paren with a trailing comma,
-- @( <itemTxt>, <existing> )@.
insertAfterOpen :: Range -> Text -> TextEdit
insertAfterOpen (Range (Position sl sc) _) itemTxt =
  TextEdit (Range pos pos) (" " <> itemTxt <> ",")
  where
    -- `sc` is the column of `(`, so insert just past it.
    pos = Position sl (sc + 1)

-- | Remove the export of @name@. Declined under CPP.
-- See Note [Reprinting erases CPP directives].
removeExport :: Maybe Rope -> ParsedSource -> RdrName -> Maybe [TextEdit]
removeExport msrc ps name =
  withExportList msrc ps (removeMatchingIE matches) declineUnderCpp
  where
    matches = parentNameIs (rdrNameFS name)

removeConstructorExport :: Maybe Rope -> RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
removeConstructorExport msrc parent ctor ps =
  withExportList msrc ps (removeCtorUnderParent parent ctor) declineUnderCpp

-- | An 'onCpp' handler that declines the edit.
-- See Note [Reprinting erases CPP directives].
declineUnderCpp :: Range -> LExportList -> Maybe [TextEdit]
declineUnderCpp _ _ = Nothing
