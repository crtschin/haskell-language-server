module Ide.Plugin.Cabal.ExposedModules (isModuleExposed) where

import           Control.Exception                             (IOException,
                                                                SomeException,
                                                                try)
import           Control.Monad.Extra                           (anyM)
import           Control.Monad.IO.Class                        (liftIO)
import qualified Data.ByteString                               as BS
import           Data.Either.Extra                             (eitherToMaybe)
import           Data.List                                     (isPrefixOf)
import           Data.Maybe                                    (maybeToList)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Development.IDE                               (Action,
                                                                NormalizedFilePath,
                                                                fromNormalizedFilePath)
import           Development.IDE.Core.Rules                    (getCradleDependencies)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (Pretty,
                                                                prettyShow)
import           Distribution.Simple.BuildTarget               (BuildTarget,
                                                                buildTargetComponentName,
                                                                readBuildTargets)
import           Distribution.Verbosity                        (silent,
                                                                verboseNoStderr)
import           Ide.Plugin.Cabal.Parse                        (parseCabalFileContents)
import           System.FilePath

-- | Whether a source file is an exposed module of a library, hence public API.
-- A multi-home-unit load can place the file under several @.cabal@ files, so we
-- treat it as exposed if any of them expose it.
--
-- Resolution failures yield 'True' so callers keep the module public and skip
-- trimming.
isModuleExposed :: NormalizedFilePath -> Text -> Action Bool
isModuleExposed nfp modName = do
  deps <- getCradleDependencies nfp
  case responsibleCabalFiles nfp deps of
    []       -> pure True
    cabalFps -> liftIO $ anyM exposedIn cabalFps
  where
    exposedIn cabalFp = do
      contents <- try (BS.readFile cabalFp) :: IO (Either IOException BS.ByteString)
      let mgpd = eitherToMaybe contents >>= eitherToMaybe . snd . parseCabalFileContents
      maybe (pure True) (\gpd -> isExposedModule cabalFp gpd (fromNormalizedFilePath nfp) modName) mgpd

-- | The @.cabal@ files from the cradle's dependencies that could own a source
-- file. A multi-home-unit load may offer several enclosing files, all of which
-- are relevant. Falls back to every @.cabal@ when none encloses the file.
responsibleCabalFiles :: NormalizedFilePath -> [FilePath] -> [FilePath]
responsibleCabalFiles nfp deps =
  if null enclosing then cabals else enclosing
  where
    fileDirs = splitDirectories (fromNormalizedFilePath nfp)
    cabals = filter ((== ".cabal") . takeExtension) deps
    enclosing =
      filter (\c -> splitDirectories (takeDirectory c) `isPrefixOf` fileDirs) cabals

isExposedModule :: FilePath -> GenericPackageDescription -> FilePath -> Text -> IO Bool
isExposedModule cabalFp gpd hsFile modName = do
  let pd = flattenPackageDescription gpd
      relPath = makeRelative (dropFileName cabalFp) hsFile
  etargets <-
    try (readBuildTargets (verboseNoStderr silent) pd [relPath]) ::
      IO (Either SomeException [BuildTarget])
  pure $ case etargets of
    Right targets@(_ : _) ->
      modName `elem` concatMap (exposedModuleNames pd . buildTargetComponentName) targets
    _ ->
      -- Cabal matches files to components relative to the process working
      -- directory, which HLS cannot rely on. When it cannot place the file,
      -- fall back to a name match against every library's exposed modules.
      modName `elem` allExposedModuleNames pd

-- | Exposed module names of the library component named by a 'ComponentName'.
-- executable, test and benchmark components have no exposed modules.
exposedModuleNames :: PackageDescription -> ComponentName -> [Text]
exposedModuleNames pd cn = case cn of
  CLibName LMainLibName -> prettyNames (maybe [] exposedModules (library pd))
  CLibName ln@(LSubLibName _) ->
    prettyNames (concatMap exposedModules (filter ((== ln) . libName) (subLibraries pd)))
  _ -> []

-- | Exposed module names of every library and sublibrary in the package.
allExposedModuleNames :: PackageDescription -> [Text]
allExposedModuleNames pd =
  prettyNames (concatMap exposedModules (maybeToList (library pd) ++ subLibraries pd))

prettyNames :: Pretty a => [a] -> [Text]
prettyNames = map (T.pack . prettyShow)
