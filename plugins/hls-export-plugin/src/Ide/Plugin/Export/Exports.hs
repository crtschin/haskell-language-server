module Ide.Plugin.Export.Exports
  ( isExplicit
  , isExported
  , isReferencedExternally
  , addExport
  , setExportList
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  )
where

import           Data.Maybe                   (isJust)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error    (srcSpanToRange)
import           Development.IDE.Types.Shake
import           HieDb
import           Ide.Plugin.Export.ExactPrint (LExportList, addCtorUnderParent,
                                               appendIE, isInIE, mkExportList,
                                               printExportList,
                                               removeCtorUnderParent,
                                               removeNamedIE, toDeltaExportList)
import           Ide.Plugin.Export.Utils
import           Language.LSP.Protocol.Types

isExplicit :: ParsedSource -> Bool
isExplicit = isJust . hsmodExports . unLoc

-- | Also matches names appearing only as constructor children of an 'IEThingWith' parent.
isExported :: RdrName -> ParsedSource -> Bool
isExported n ps = case hsmodExports (unLoc ps) of
  Nothing      -> False
  Just (L _ items) -> any (covers . unLoc) items
  where
    nFS = rdrNameFS n
    covers ie =
      maybe False ((== nFS) . rdrNameFS) (ieParentName ie)
        || isInIE nFS ie

addExportList :: ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
addExportList ps items = do
  lmodName <- hsmodName (unLoc ps)
  Range _ end <- srcSpanToRange (getLoc lmodName)
  let listText = printExportList (mkExportList items)
  Just [TextEdit (Range end end) (" " <> listText)]

setExportList :: ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
setExportList ps items = case hsmodExports (unLoc ps) of
  Nothing -> addExportList ps items
  Just (L lloc _) -> do
    r <- srcSpanToRange (locA lloc)
    let listText = printExportList (mkExportList items)
    Just [TextEdit r listText]

replaceExportList :: ParsedSource -> (LExportList -> Maybe LExportList) -> Maybe [TextEdit]
replaceExportList ps f = do
  exports <- hsmodExports (unLoc ps)
  newList <- f (toDeltaExportList exports)
  r <- srcSpanToRange (getLoc exports)
  Just [TextEdit r (printExportList newList)]

addExport :: ParsedSource -> LIE GhcPs -> Maybe [TextEdit]
addExport ps item = replaceExportList ps (Just . appendIE item)

addConstructorExport :: RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
addConstructorExport parent ctor ps = replaceExportList ps (addCtorUnderParent parent ctor)

removeExport :: ParsedSource -> RdrName -> Maybe [TextEdit]
removeExport ps name = replaceExportList ps (removeNamedIE name)

removeConstructorExport :: RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
removeConstructorExport parent ctor ps = replaceExportList ps (removeCtorUnderParent parent ctor)

isReferencedExternally :: WithHieDb -> [FilePath] -> AvailInfo -> IO Bool
isReferencedExternally withDb exclude avail = do
  let n = availName avail
  case nameModule_maybe n of
    Nothing  -> pure False
    Just modl -> do
      rows <- withDb $ \db ->
        findReferences db True (nameOccName n) (Just (moduleName modl)) (Just (moduleUnit modl)) exclude
      pure (not (null rows))
