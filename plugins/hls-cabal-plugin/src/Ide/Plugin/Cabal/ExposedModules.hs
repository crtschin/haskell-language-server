{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

-- | What a package's cabal file says about the modules it builds, and which
-- component owns a given source file.
module Ide.Plugin.Cabal.ExposedModules
  ( exposureCheck
  , isModuleExposed
  , exposedModulesRules
  , Log (..)
    -- * Rules
  , GetExposedModules (..)
  , GetResponsibleCabalFile (..)
  , ComponentModules
    -- * Exposed for testing
  , exposedBy
  , packageModules
  ) where

import           Control.DeepSeq                               (NFData)
import           Control.Monad                                 (filterM)
import           Control.Monad.IO.Class                        (liftIO)
import           Data.Hashable                                 (Hashable)
import           Data.List                                     (isPrefixOf)
import           Data.Maybe                                    (catMaybes,
                                                                maybeToList)
import           Data.Set                                      (Set)
import qualified Data.Set                                      as Set
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Development.IDE
import qualified Development.IDE.Core.Shake                    as Shake
import           Distribution.ModuleName                       (ModuleName)
import qualified Distribution.ModuleName                       as ModuleName
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (prettyShow)
import           Distribution.Types.Component                  (componentBuildInfo,
                                                                foldComponent)
import           Distribution.Utils.Path                       (getSymbolicPath)
import           GHC.Generics                                  (Generic)
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalFile (..))
import           Ide.Plugin.Cabal.Files                        (findResponsibleCabalFile)
import           System.Directory                              (canonicalizePath,
                                                                doesFileExist)
import           System.FilePath

newtype Log = LogShake Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty (LogShake msg) = pretty msg

-- | One buildable component, reduced to what callers actually ask about.
data ComponentModules = ComponentModules
  { cmSourceDirs :: [FilePath]
    -- ^ Canonicalised absolute @hs-source-dirs@. An absent field means the
    -- package root, so this is never empty.
  , cmExposed    :: Set Text
    -- ^ Public API. Empty for anything but a publicly visible library.
  , cmFiles      :: [NormalizedFilePath]
    -- ^ Existing source files for every module the component builds, @main-is@
    -- included. Held as paths because every @main-is@ module is called @Main@,
    -- so names would let one component stand in for another.
    --
    -- Normalised rather than canonicalised, to match the session's view, which
    -- does not resolve symlinks.
  }
  deriving (Show, Generic)

instance NFData ComponentModules

data GetExposedModules = GetExposedModules
  deriving (Eq, Show, Generic)

instance Hashable GetExposedModules

instance NFData GetExposedModules

type instance RuleResult GetExposedModules = [ComponentModules]

data GetResponsibleCabalFile = GetResponsibleCabalFile
  deriving (Eq, Show, Generic)

instance Hashable GetResponsibleCabalFile

instance NFData GetResponsibleCabalFile

-- | At most one entry. A list rather than a 'Maybe' so that "no cabal file"
-- stays an ordinary result instead of a rule failure.
type instance RuleResult GetResponsibleCabalFile = [NormalizedFilePath]

exposedModulesRules :: Recorder (WithPriority Log) -> Rules ()
exposedModulesRules recorder = do
  defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetExposedModules file -> do
    mGpd <- use ParseCabalFile file
    traverse (liftIO . packageModules file . flattenPackageDescription) mGpd

  defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetResponsibleCabalFile file -> do
    mCabal <- liftIO $ findResponsibleCabalFile (fromNormalizedFilePath file)
    pure $ Just (map toNormalizedFilePath' (maybeToList mCabal))

-- | Whether the module is public API, and every file the cabal file says gets
-- built. One lookup because the caller needs both on the same request.
exposureCheck :: NormalizedFilePath -> Text -> IdeAction (Bool, [NormalizedFilePath])
exposureCheck nfp modName = do
  comps <- componentsFor nfp
  canonicalFp <- liftIO $ canonicalizePath (fromNormalizedFilePath nfp)
  pure (exposedBy canonicalFp modName comps, concatMap cmFiles comps)

-- | Whether the module is public API of a library that owns the file.
isModuleExposed :: NormalizedFilePath -> Text -> IdeAction Bool
isModuleExposed nfp modName = fst <$> exposureCheck nfp modName

-- | The components of the package owning @nfp@, or none when no cabal file
-- governs it. hpack projects get none, since 'findResponsibleCabalFile'
-- declines a generated cabal file.
componentsFor :: NormalizedFilePath -> IdeAction [ComponentModules]
componentsFor nfp = do
  cabalFps <- maybe [] fst <$> useWithStaleFast GetResponsibleCabalFile nfp
  concat . catMaybes <$> traverse (fmap (fmap fst) . useWithStaleFast GetExposedModules) cabalFps

-- | Defaults to 'True' when no component owns the file, so an undecidable case
-- withholds the action rather than offering it on a published interface.
exposedBy :: FilePath -> Text -> [ComponentModules] -> Bool
exposedBy canonicalFp modName comps = case owningComponents canonicalFp comps of
  []     -> True
  owners -> any (Set.member modName . cmExposed) owners

owningComponents :: FilePath -> [ComponentModules] -> [ComponentModules]
owningComponents canonicalFp comps = [c | (depth, c) <- matches, depth == deepest]
  where
    -- A real match always has depth >= 1, so 0 covers the no-match case.
    deepest = maximum (0 : map fst matches)
    matches =
      [ (length (splitDirectories dir), c)
      | c <- comps
      , dir <- cmSourceDirs c
      , splitDirectories dir `isPrefixOf` splitDirectories canonicalFp
      ]

packageModules :: NormalizedFilePath -> PackageDescription -> IO [ComponentModules]
packageModules cabalFp pd = traverse resolve (buildComponents pd)
  where
    root = takeDirectory (fromNormalizedFilePath cabalFp)
    resolve (bi, exposed, relFiles) = do
      let dirs = sourceDirsOf bi
      canonical <- traverse (canonicalizePath . (root </>)) dirs
      present <- filterM doesFileExist [root </> d </> f | d <- dirs, f <- relFiles]
      pure ComponentModules
        { cmSourceDirs = canonical
        , cmExposed = Set.fromList (map moduleText exposed)
        , cmFiles = map toNormalizedFilePath' present
        }

-- | Each component as @(build info, exposed modules, source-dir-relative files)@.
--
-- Non-library components contribute no exports but do contribute source dirs,
-- so a file is attributed to the component that actually builds it.
buildComponents :: PackageDescription -> [(BuildInfo, [ModuleName], [FilePath])]
buildComponents pd = [(componentBuildInfo c, exposed c, files c) | c <- pkgComponents pd]
  where
    exposed = foldComponent publicLib none none none none
    publicLib l
      | libVisibility l == LibraryVisibilityPublic = exposedModules l
      | otherwise                                  = []
    none = const []
    files = foldComponent
      (moduleFiles . explicitLibModules)
      (moduleFiles . foreignLibModules)
      (\e -> mainIs (modulePath e) : moduleFiles (exeModules e))
      (\t -> testMain (testInterface t) ++ moduleFiles (testModules t))
      (moduleFiles . benchmarkModules)
    testMain (TestSuiteExeV10 _ p) = [mainIs p]
    testMain _                     = []
#if MIN_VERSION_Cabal_syntax(3,12,0)
    mainIs = getSymbolicPath
#else
    mainIs = id
#endif

moduleFiles :: [ModuleName] -> [FilePath]
moduleFiles ms = [ModuleName.toFilePath m <.> ext | m <- ms, ext <- ["hs", "lhs"]]

sourceDirsOf :: BuildInfo -> [FilePath]
sourceDirsOf bi = case map getSymbolicPath (hsSourceDirs bi) of
  []   -> ["."]
  dirs -> dirs

moduleText :: ModuleName -> Text
moduleText = T.pack . prettyShow
