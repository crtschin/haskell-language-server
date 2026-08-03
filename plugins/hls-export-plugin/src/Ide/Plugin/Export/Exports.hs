module Ide.Plugin.Export.Exports
  ( isExplicit
  , isExported
  , hasModuleReexport
  , exportListHasCpp
  , addExport
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  , retainExports
  , addExportList
  , isReferencedExternally
  ) where

import           Control.Monad.Extra                (anyM)
import           Data.Maybe                         (isJust)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Utf16.Rope.Mixed         (Rope)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util    (FastString)
import           Development.IDE.GHC.Error          (srcSpanToRange)
import           Development.IDE.GHC.ExactPrint.CPP (spanHasCpp)
import           Development.IDE.Types.Shake        (WithHieDb)
import           HieDb                              (findReferences,
                                                     modInfoName, (:.) (..))
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

-- | Extract the export list and pick an edit strategy. Under CPP it splices
-- surgically, otherwise it reprints the whole transformed list.
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

-- | Drop the export entries whose head name is absent from @keep@, leaving
-- survivors exactly as the author wrote them. 'Nothing' when nothing matches or
-- the list holds a directive. See Note [Reprinting erases CPP directives].
retainExports :: Maybe Rope -> ParsedSource -> [FastString] -> Maybe [TextEdit]
retainExports msrc ps keep =
  withExportList msrc ps (removeAllMatchingIE dropped) declineUnderCpp
  where
    -- Only `module M` re-exports and doc items lack a head name, and the
    -- caller already refuses those.
    dropped = maybe False ((`notElem` keep) . rdrNameFS) . ieParentName

-- | Splice a fresh @( item, ... )@ list in right after the module header.
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

{- Note [Definition sites count as references]

hiedb records a definition site as a reference like any other, so a re-exported
name always has rows in its origin. Discarding those is what lets a dead
re-export be trimmed at all.

A consumer that reached the name through this module stays indistinguishable
from one that imported the origin directly, so a re-export whose origin is
imported elsewhere is kept. That errs towards keeping an export.
-}

-- | Keep an export if any name it brings into scope is referenced from outside
-- both this module and the module that defines the name. A @T(..)@ child can be
-- used without @T@ itself, so every member is checked.
--
-- See Note [Definition sites count as references].
isReferencedExternally :: WithHieDb -> [FilePath] -> AvailInfo -> IO Bool
isReferencedExternally withDb exclude avail = anyM referenced (availNames avail)
  where
    referenced n = case nameModule_maybe n of
      Nothing  -> pure False
      Just mod -> do
        rows <- withDb $ \db ->
          findReferences db True (nameOccName n) (Just (moduleName mod)) (Just (moduleUnit mod)) exclude
        pure (any (not . definedBy (moduleName mod)) rows)
    definedBy mn (_ :. info) = modInfoName info == mn

-- | Splice @itemTxt@ in right after the opening paren with a trailing comma,
-- @( <itemTxt>, <existing> )@.
insertAfterOpen :: Range -> Text -> TextEdit
insertAfterOpen (Range (Position sl sc) _) itemTxt =
  TextEdit (Range pos pos) (" " <> itemTxt <> ",")
  where
    -- `sc` is the column of `(`, so insert just past it.
    pos = Position sl (sc + 1)

-- | Remove the export of @name@. Declined under CPP.
removeExport :: Maybe Rope -> ParsedSource -> RdrName -> Maybe [TextEdit]
removeExport msrc ps name =
  withExportList msrc ps (removeMatchingIE matches) declineUnderCpp
  where
    matches = parentNameIs (rdrNameFS name)

removeConstructorExport :: Maybe Rope -> RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
removeConstructorExport msrc parent ctor ps =
  withExportList msrc ps (removeCtorUnderParent parent ctor) declineUnderCpp

-- | An 'onCpp' handler that declines the edit.
declineUnderCpp :: Range -> LExportList -> Maybe [TextEdit]
declineUnderCpp _ _ = Nothing
