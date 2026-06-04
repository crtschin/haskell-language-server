module Ide.Plugin.Cabal.ExposedModules
  ( ExposedModuleCheck (..)
  , exposedModuleCheck
  ) where

import           Control.Exception                             (IOException,
                                                                try)
import qualified Data.ByteString                               as BS
import           Data.Maybe                                    (maybeToList)
import qualified Data.Text                                     as T
import           Development.IDE                               (NormalizedFilePath,
                                                                fromNormalizedFilePath)
import           Distribution.PackageDescription               (PackageDescription,
                                                                exposedModules,
                                                                libBuildInfo,
                                                                library,
                                                                subLibraries)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (prettyShow)
import           Ide.Plugin.Cabal.CabalAdd.CodeAction          (buildInfoToHsSourceDirs,
                                                                mkRelativeModulePathM)
import           Ide.Plugin.Cabal.Files                        (findNearestCabalFile)
import           Ide.Plugin.Cabal.Parse                        (parseCabalFileContents)

-- | Whether a module is exposed by the package library or a sublibrary, hence public API.
newtype ExposedModuleCheck = ExposedModuleCheck
  { isExposed :: NormalizedFilePath -> Bool }

-- | Reads the nearest enclosing @.cabal@. A missing or unparseable file yields 'const True', so callers treat every module as public and skip trimming.
exposedModuleCheck :: NormalizedFilePath -> IO ExposedModuleCheck
exposedModuleCheck target = do
  mcabal <- findNearestCabalFile (fromNormalizedFilePath target)
  case mcabal of
    Nothing      -> pure alwaysExposed
    Just cabalFp -> do
      contents <- try (BS.readFile cabalFp) :: IO (Either IOException BS.ByteString)
      pure $ case contents of
        Left _   -> alwaysExposed
        Right bs -> case snd (parseCabalFileContents bs) of
          Left _    -> alwaysExposed
          Right gpd ->
            let pd = flattenPackageDescription gpd
             in ExposedModuleCheck (isExposedModule cabalFp pd . fromNormalizedFilePath)
  where
    alwaysExposed = ExposedModuleCheck (const True)

isExposedModule :: FilePath -> PackageDescription -> FilePath -> Bool
isExposedModule cabalFp pd hsFile =
    any inLibrary (maybeToList (library pd) ++ subLibraries pd)
  where
    inLibrary lib = case mkRelativeModulePathM srcDirs cabalFp hsFile of
        Just modPath -> modPath `elem` exposed
        Nothing      -> False
      where
        srcDirs = case buildInfoToHsSourceDirs (libBuildInfo lib) of
          []   -> ["."]
          dirs -> dirs
        exposed = map (T.pack . prettyShow) (exposedModules lib)
