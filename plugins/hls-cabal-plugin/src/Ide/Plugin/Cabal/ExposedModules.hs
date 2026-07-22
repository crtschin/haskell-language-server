module Ide.Plugin.Cabal.ExposedModules
  ( isModuleExposed
    -- * Exposed for testing
  , packageOwns
  ) where

import           Control.Monad                                 (filterM)
import           Control.Monad.IO.Class                        (liftIO)
import           Data.List                                     (isPrefixOf)
import           Data.Maybe                                    (maybeToList)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Development.IDE                               (NormalizedFilePath,
                                                                fromNormalizedFilePath,
                                                                toNormalizedFilePath')
import           Development.IDE.Core.RuleTypes                (GhcSessionIO (..),
                                                                IdeGhcSession (..))
import           Development.IDE.Core.Shake                    (IdeAction,
                                                                useWithStaleFast)
import           Development.IDE.Types.Location                (emptyFilePath)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (prettyShow)
import           Distribution.Utils.Path                       (getSymbolicPath)
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalFile (..))
import           System.Directory                              (canonicalizePath)
import           System.FilePath

-- | Whether a source file is an exposed module of a library.
isModuleExposed :: NormalizedFilePath -> Text -> IdeAction Bool
isModuleExposed nfp modName = do
  deps <- cradleDependencies nfp
  let cabalFps = filter ((== ".cabal") . takeExtension) deps
  pkgs <- traverse (\fp -> (fp,) . fmap (flattenPackageDescription . fst)
                             <$> useWithStaleFast ParseCabalFile (toNormalizedFilePath' fp)) cabalFps
  liftIO $ packageDescExposes nfp modName pkgs

-- | The file's cradle dependencies via the loaded session.
cradleDependencies :: NormalizedFilePath -> IdeAction [FilePath]
cradleDependencies nfp = do
  mSession <- useWithStaleFast GhcSessionIO emptyFilePath
  case mSession of
    Nothing           -> pure []
    Just (session, _) -> liftIO $ snd <$> loadSessionFun session (fromNormalizedFilePath nfp)

-- | Whether the file is exposed by any package that owns it. A file can belong
-- to more than one package, so we treat it as exposed if any owning package
-- exposes it. Defaults to 'True'.
packageDescExposes :: NormalizedFilePath -> Text -> [(FilePath, Maybe PackageDescription)] -> IO Bool
packageDescExposes nfp modName pkgs = do
  canonicalFp <- canonicalizePath (fromNormalizedFilePath nfp)
  -- Prefer the packages whose source dirs actually contain the file. Fallback
  -- to checking every package.
  owning <- filterM (\(fp, mpd) -> maybe (pure True) (packageOwns canonicalFp fp) mpd) pkgs
  let relevant = if null owning then pkgs else owning
  pure $ null relevant || any (maybe True (`exposesModule` modName) . snd) relevant

-- | Whether the package could compile the file, i.e. it lives under one of the
-- package's @hs-source-dirs@.
packageOwns :: FilePath -> FilePath -> PackageDescription -> IO Bool
packageOwns canonicalFp cabalFp pd = do
  sourceDirs <- traverse (canonicalizePath . (takeDirectory cabalFp </>)) rawDirs
  pure $ any (\d -> splitDirectories d `isPrefixOf` splitDirectories canonicalFp) sourceDirs
  where
    rawDirs = concatMap buildInfoDirs (allBuildInfo pd)
    -- An absent hs-source-dirs means the package root.
    buildInfoDirs bi = case map getSymbolicPath (hsSourceDirs bi) of
      []   -> ["."]
      dirs -> dirs

-- | Whether the package exposes a library module of this name.
exposesModule :: PackageDescription -> Text -> Bool
exposesModule pd modName = modName `elem` allExposedModuleNames pd

-- | Exposed module names of every library and sublibrary in the package.
allExposedModuleNames :: PackageDescription -> [Text]
allExposedModuleNames pd =
  map (T.pack . prettyShow) $ concatMap exposedModules (maybeToList (library pd) ++ subLibraries pd)
