module Ide.Plugin.Export.Exports
  ( Source (..)
  , mkSource
  , getSource
  , addExport
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  ) where

import           Control.Applicative                ((<|>))
import           Control.Monad.Trans.Except         (ExceptT)
import           Data.Text                          (Text)
import           Data.Text.Utf16.Rope.Mixed         (Rope)
import           Development.IDE
import           Development.IDE.Core.PluginUtils   (useE)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint.CPP (spanHasCpp)
import           Ide.Plugin.Error                   (PluginError)
import           Ide.Plugin.Export.Cursor           (ExportFlavor)
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Utils
import           Language.Haskell.GHC.ExactPrint    (makeDeltaAst)
import           Language.LSP.Protocol.Types

data Source = Source
  { parsed :: ParsedSource
  , buffer :: Maybe Rope
    -- ^ The parser strips CPP directives, so we pull them from the buffer.
  , dflags :: DynFlags
  }

mkSource :: Monad m => m (Maybe Rope) -> ParsedModule -> m (Maybe Source)
mkSource fetchBuffer pm
  -- Only a module that uses CPP needs the buffer.
  | isCppModule summ = fmap (source . Just) <$> fetchBuffer
  | otherwise        = pure (Just (source Nothing))
  where
    summ = pm_mod_summary pm
    source buffer = Source (pm_parsed_source pm) buffer (ms_hspp_opts summ)

getSource :: NormalizedFilePath -> ExceptT PluginError Action (Maybe Source)
getSource nfp = do
  pm <- useE GetParsedModuleWithComments nfp
  mkSource (snd <$> useE GetFileContents nfp) pm

isExported :: RdrName -> ParsedSource -> Bool
isExported n ps = case hsmodExports (unLoc ps) of
  Nothing          -> False
  Just (L _ items) -> any ((== rdrNameFS n) . rdrNameFS) (concatMap (ieNames . unLoc) items)

-- | When the list holds a CPP directive, inserts the splice's entry after the
-- opening parenthesis, otherwise reprints the whole export list.
withExportList
  :: Source
  -> (LExportList -> Maybe LExportList)
  -> Maybe (LExportList -> Maybe Text)
  -> Maybe [TextEdit]
withExportList src reprint splice = do
  exports <- hsmodExports (unLoc src.parsed)
  full@(Range (Position sl sc) _) <- srcSpanToRange (getLoc exports)
  if spanHasCpp src.buffer full
    then do
      entry <- splice >>= ($ exports)
      -- `sc` is the column of `(`.
      let pos = Position sl (sc + 1)
      Just [TextEdit (Range pos pos) (" " <> entry <> ",")]
    else do
      newList <- reprint (makeDeltaAst exports)
      Just [TextEdit full (printExportList newList)]

addExport :: Source -> ExportFlavor -> RdrName -> Maybe [TextEdit]
addExport src flavor n
  | n `isExported` src.parsed = Nothing
  | otherwise = appendExport src (renderExport flavor n)

appendExport :: Source -> Text -> Maybe [TextEdit]
appendExport src item =
  withExportList src (\l -> (`appendIE` l) <$> parseIE src.dflags item) (Just (const (Just item)))

addConstructorExport :: Source -> RdrName -> RdrName -> Maybe [TextEdit]
addConstructorExport src parent ctor
  | ctor `isExported` src.parsed = Nothing
  | otherwise =
      withExportList src (addCtorUnderParent src.dflags parent ctor)
        (Just (freshCtorEntry parent ctor . unLoc))

removeExport :: Source -> RdrName -> Maybe [TextEdit]
removeExport src name =
  -- No splice, so no unexporting when the export list holds a directive. It
  -- gets edge-casey trying to do this safely.
  withExportList src (removeMatchingIE (parentNameIs (rdrNameFS name))) Nothing

removeConstructorExport :: Source -> RdrName -> RdrName -> Maybe [TextEdit]
removeConstructorExport src parent ctor =
  withExportList src (removeCtorUnderParent src.dflags parent ctor) Nothing
    -- Fall back to a standalone entry for the constructor. A bare uppercase
    -- entry denotes the type, so skip a constructor named like its type.
    <|> if rdrNameFS ctor == rdrNameFS parent then Nothing else removeExport src ctor
