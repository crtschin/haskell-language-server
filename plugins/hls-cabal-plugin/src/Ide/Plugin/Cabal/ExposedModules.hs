{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | What a package's cabal file says about the modules it builds, and which
-- component owns a given source file.
module Ide.Plugin.Cabal.ExposedModules
  ( exposureCheckWith,
    ComponentModules,
    exposedModulesRules,

    -- * Exposed for testing
    exposedBy,
    packageModules,
  )
where

import           Control.DeepSeq                               (NFData)
import           Control.Monad                                 (join)
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import qualified Data.ByteString                               as BS
import           Data.Hashable                                 (Hashable)
import           Data.List                                     (isPrefixOf)
import           Data.Maybe                                    (fromMaybe)
import           Data.Set                                      (Set)
import qualified Data.Set                                      as Set
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Data.Text.Encoding                            (encodeUtf8)
import           Development.IDE
import qualified Development.IDE.Core.Shake                    as Shake
import           Development.IDE.Graph                         (alwaysRerun)
import           Distribution.ModuleName                       (ModuleName)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (prettyShow)
import           Distribution.Types.Component
import           Distribution.Utils.Path                       (getSymbolicPath)
import           GHC.Generics                                  (Generic)
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalFile (..))
import           Ide.Plugin.Cabal.Files                        (findResponsibleCabalFile)
import           System.Directory.OsPath                       (canonicalizePath)
import qualified System.FilePath                               as FP
import           System.OsPath

-- | One buildable component, reduced to what callers ask about.
data ComponentModules = ComponentModules
  { -- | Absolute @hs-source-dirs@.
    sourceDirs :: [OsPath],
    -- | Public API
    exposed    :: Set Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data GetExposedModules = GetExposedModules
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
type instance RuleResult GetExposedModules = [ComponentModules]

data GetResponsibleCabalFile = GetResponsibleCabalFile
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
type instance RuleResult GetResponsibleCabalFile = Maybe NormalizedFilePath

exposedModulesRules :: Recorder (WithPriority Shake.Log) -> Rules ()
exposedModulesRules recorder = do
  defineNoDiagnostics recorder $ \GetExposedModules file -> do
    mGpd <- use ParseCabalFile file
    traverse (liftIO . packageModules file . flattenPackageDescription) mGpd

  -- Keyed on the directory, so it's shared between sibling files.
  defineEarlyCutoff recorder $ RuleNoDiagnostics $ \GetResponsibleCabalFile dir -> do
    alwaysRerun
    mCabal <- liftIO $ findResponsibleCabalFile (FP.addTrailingPathSeparator (fromNormalizedFilePath dir))
    let res = toNormalizedFilePath' <$> mCabal
    pure (Just (summarize res), Just res)

type Lookup m = forall k v. (IdeRule k v) => k -> NormalizedFilePath -> m (Maybe v)

exposureCheckWith :: (MonadIO m) => Lookup m -> NormalizedFilePath -> Text -> m Bool
exposureCheckWith look nfp modName = do
  comps <- componentsWith look nfp
  canonicalFp <- liftIO $ canonicalizePath =<< encodeFS (fromNormalizedFilePath nfp)
  pure (exposedBy canonicalFp modName comps)

-- | The components of the package owning @nfp@, or none when no cabal file.
componentsWith :: (Monad m) => Lookup m -> NormalizedFilePath -> m [ComponentModules]
componentsWith look nfp = do
  let dir = toNormalizedFilePath' (FP.takeDirectory (fromNormalizedFilePath nfp))
  mCabalFp <- join <$> look GetResponsibleCabalFile dir
  case mCabalFp of
    Nothing      -> pure []
    Just cabalFp -> fromMaybe [] <$> look GetExposedModules cabalFp

summarize :: Maybe NormalizedFilePath -> BS.ByteString
summarize = maybe BS.empty (encodeUtf8 . T.pack . fromNormalizedFilePath)

-- | Defaults to 'True' when no component owns the file.
exposedBy :: OsPath -> Text -> [ComponentModules] -> Bool
exposedBy canonicalFp modName comps = case owningComponents canonicalFp comps of
  []     -> True
  owners -> any (Set.member modName . (.exposed)) owners

owningComponents :: OsPath -> [ComponentModules] -> [ComponentModules]
owningComponents canonicalFp comps = [c | (depth, c) <- matches, depth == deepest]
  where
    -- A real match always has depth >= 1, so 0 covers the no-match case.
    deepest = maximum (0 : map fst matches)
    fileDirs = splitDirectories canonicalFp
    matches =
      [ (length dirParts, c)
      | c <- comps,
        dir <- c.sourceDirs,
        let dirParts = splitDirectories dir,
        dirParts `isPrefixOf` fileDirs
      ]

packageModules :: NormalizedFilePath -> PackageDescription -> IO [ComponentModules]
packageModules cabalFp pd = do
  root <- takeDirectory <$> encodeFS (fromNormalizedFilePath cabalFp)
  traverse (resolve root) (buildComponents pd)
  where
    resolve root (bi, exposed) = do
      dirs <- sourceDirsOf bi
      sourceDirs <- traverse (canonicalizePath . (root </>)) dirs
      pure
        ComponentModules
          { exposed = Set.fromList (map moduleText exposed),
            ..
          }

-- | Every component contributes its source dirs, and a public library
-- contributes exports.
buildComponents :: PackageDescription -> [(BuildInfo, [ModuleName])]
buildComponents pd = [(componentBuildInfo c, exposed c) | c <- pkgComponents pd]
  where
    exposed = foldComponent publicLib none none none none
    publicLib l
      | libVisibility l == LibraryVisibilityPublic = exposedModules l
      | otherwise = []
    none = const []

sourceDirsOf :: BuildInfo -> IO [OsPath]
sourceDirsOf bi = traverse encodeFS $ case map getSymbolicPath (hsSourceDirs bi) of
  []   -> ["."]
  dirs -> dirs

moduleText :: ModuleName -> Text
moduleText = T.pack . prettyShow
