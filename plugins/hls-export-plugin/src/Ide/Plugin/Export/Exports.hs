module Ide.Plugin.Export.Exports
  ( isExplicit
  , isExported
  , setExportListExpanding
  , addExport
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  ) where

import           Data.List                          (find, findIndex)
import           Data.Maybe                         (catMaybes, isJust,
                                                     listToMaybe)
import           Data.Text                          (Text)
import           Data.Text.Utf16.Rope.Mixed         (Rope)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util    (FastString)
import           Development.IDE.GHC.Error          (srcSpanToRange)
import           Development.IDE.GHC.ExactPrint.CPP (spanHasCpp)
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Utils
import           Language.Haskell.GHC.ExactPrint    (makeDeltaAst)
import           Language.LSP.Protocol.Types

isExplicit :: ParsedSource -> Bool
isExplicit = isJust . hsmodExports . unLoc

-- | Also matches names appearing only as constructor children of an 'IEThingWith' parent.
isExported :: RdrName -> ParsedSource -> Bool
isExported n ps = case hsmodExports (unLoc ps) of
  Nothing          -> False
  Just (L _ items) -> any (covers . unLoc) items
  where
    nFS = rdrNameFS n
    covers ie = parentNameIs nFS ie || isInIE nFS ie

addExportList :: ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
addExportList ps items = do
  lmodName <- hsmodName (unLoc ps)
  Range _ end <- srcSpanToRange (getLoc lmodName)
  let listText = printExportList (mkExportList items)
  Just [TextEdit (Range end end) (" " <> listText)]

-- | Additive: an existing list keeps every entry and only gains the missing
-- items, so a partial list is expanded in place.
setExportListExpanding :: ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
setExportListExpanding = setExportListWith expandExportList

-- | Replace the export list with @merge items@ applied to the current one,
-- adding a fresh list after the module name when there is none yet.
setExportListWith :: ([LIE GhcPs] -> LExportList -> LExportList) -> ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
setExportListWith merge ps items = case hsmodExports (unLoc ps) of
  Nothing      -> addExportList ps items
  Just exports -> do
    r <- srcSpanToRange (getLoc exports)
    Just [TextEdit r (printExportList (merge items (makeDeltaAst exports)))]

-- | Extract the export list and pick an edit strategy: splice surgically when
-- the span holds a CPP directive, otherwise reprint the whole transformed list.
withExportList
  :: Maybe Rope
  -> ParsedSource
  -> (LExportList -> Maybe LExportList)          -- ^ reprint transform
  -> (Range -> LExportList -> Maybe [TextEdit])  -- ^ list holds a directive
  -> Maybe [TextEdit]
withExportList msrc ps reprint onCpp = do
  exports <- hsmodExports (unLoc ps)
  full <- srcSpanToRange (getLoc exports)
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

-- | Splice @itemTxt@ in right after the opening paren with a trailing comma,
-- @( <itemTxt>, <existing> )@. Valid in every CPP branch: a first item needs no
-- leading separator and a trailing comma is always legal.
insertAfterOpen :: Range -> Text -> TextEdit
insertAfterOpen (Range (Position sl sc) _) itemTxt =
  TextEdit (Range pos pos) (" " <> itemTxt <> ",")
  where
    -- `sc` is the column of `(`, so insert just past it.
    pos = Position sl (sc + 1)

-- | Removal reprints the transformed list, which would drop the directives the
-- parser stripped. Under CPP fall back to a surgical text delete of the targeted
-- entry, offered only when a directive-free side exists ('deleteOneClean').
removeExport :: Maybe Rope -> ParsedSource -> RdrName -> Maybe [TextEdit]
removeExport msrc ps name =
  withExportList msrc ps (removeNamedIE name) $ \_ (L _ items) ->
    fmap (:[]) (deleteOneClean msrc getLocA (parentNameIs (rdrNameFS name) . unLoc) items)

removeConstructorExport :: Maybe Rope -> RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
removeConstructorExport msrc parent ctor ps =
  withExportList msrc ps (removeCtorUnderParent parent ctor) $ \_ (L _ items) ->
    surgicalRemoveCtor msrc (rdrNameFS parent) (rdrNameFS ctor) items

-- | Drop one constructor child from the first matching @T(...)@ entry. Declines
-- the only-child case (a text delete cannot downgrade @T(C)@ to @T@) and any
-- non-@IEThingWith@ shape, matching the reprint path's CPP refusal for those.
surgicalRemoveCtor :: Maybe Rope -> FastString -> FastString -> [LIE GhcPs] -> Maybe [TextEdit]
surgicalRemoveCtor msrc parentFS ctorFS items = do
  L _ ie <- find (parentNameIs parentFS . unLoc) items
  children <- ieThingWithChildren ie
  if length children < 2
    then Nothing
    else fmap (:[]) (deleteOneClean msrc getLocA ((== ctorFS) . lieWrappedNameFS) children)

-- | Surgically delete the one matching element from a located list, consuming
-- the separator on whichever neighbouring side is free of a CPP directive.
-- Prefers the following side, which leaves a preceding item's trailing comment
-- untouched. 'Nothing' when nothing matches, or no directive-free side exists
-- (sole element, or both usable gaps cross a directive). This always removes
-- exactly one element and one comma, so the result is well formed in every CPP
-- configuration.
deleteOneClean :: Maybe Rope -> (a -> SrcSpan) -> (a -> Bool) -> [a] -> Maybe TextEdit
deleteOneClean msrc spanOf match items = do
  i <- findIndex match items
  Range tStart tEnd <- srcSpanToRange (spanOf (items !! i))
  let following = do
        next <- items !!? (i + 1)
        Range nStart _ <- srcSpanToRange (spanOf next)
        Just (Range tStart nStart)
      preceding = do
        prev <- items !!? (i - 1)
        Range _ pEnd <- srcSpanToRange (spanOf prev)
        Just (Range pEnd tEnd)
  range <- listToMaybe (filter (not . spanHasCpp msrc) (catMaybes [following, preceding]))
  Just (TextEdit range mempty)
  where
    xs !!? n
      | n < 0 || n >= length xs = Nothing
      | otherwise               = Just (xs !! n)
