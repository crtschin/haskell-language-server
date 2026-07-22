module ExposedModules (exposedModulesTests) where

import           Data.List                                     (intercalate)
import qualified Data.Text                                     as T
import           Data.Text.Encoding                            (encodeUtf8)
import           Distribution.PackageDescription               (PackageDescription)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Ide.Plugin.Cabal.ExposedModules               (packageOwns)
import qualified Ide.Plugin.Cabal.Parse                        as Parse
import           System.Directory                              (createDirectoryIfMissing)
import           System.FilePath                               ((</>))
import           Test.Hls

exposedModulesTests :: TestTree
exposedModulesTests = testGroup "packageOwns"
  [ ownsCase "owns a file under a declared source dir"
      ["pkg/src"] "pkg/src/Foo.hs" "pkg/pkg.cabal" ["src"] True
  , ownsCase "does not own a file outside every source dir"
      ["pkg/src"] "pkg/app/Foo.hs" "pkg/pkg.cabal" ["src"] False
  , ownsCase "absent hs-source-dirs means the package root"
      ["pkg"] "pkg/Foo.hs" "pkg/pkg.cabal" [] True
  , ownsCase "an ancestor package does not own a nested package's file"
      ["src", "sub/src"] "sub/src/Foo.hs" "outer.cabal" ["src"] False
  , ownsCase "out-of-tree source dir (..) is resolved and owned"
      ["pkgA", "shared"] "shared/Foo.hs" "pkgA/pkgA.cabal" ["../shared"] True
  ]

ownsCase :: TestName -> [FilePath] -> FilePath -> FilePath -> [String] -> Bool -> TestTree
ownsCase name dirs file cabal srcDirs want =
  testCase name $ withCanonicalTempDir $ \tmp -> do
    mapM_ (createDirectoryIfMissing True . (tmp </>)) dirs
    owned <- packageOwns (tmp </> file) (tmp </> cabal) (pkgWith srcDirs)
    (owned == want) @? (file <> ": expected packageOwns=" <> show want <> ", got " <> show owned)

pkgWith :: [String] -> PackageDescription
pkgWith dirs =
  case snd (Parse.parseCabalFileContents (encodeUtf8 (T.pack cabalText))) of
    Right gpd -> flattenPackageDescription gpd
    Left err  -> error ("packageOwns test: could not parse cabal: " <> show err)
  where
    cabalText = unlines $
      [ "cabal-version: 2.4"
      , "name: p"
      , "version: 0.1"
      , "library"
      , "  exposed-modules: Foo"
      , "  build-depends: base"
      , "  default-language: Haskell2010"
      ]
      ++ [ "  hs-source-dirs: " <> intercalate ", " dirs | not (null dirs) ]
