{-# LANGUAGE OverloadedStrings #-}

module ExposedModules (exposedModulesTests) where

import           Data.List                                     (intercalate)
import qualified Data.Text                                     as T
import           Data.Text.Encoding                            (encodeUtf8)
import           Development.IDE                               (toNormalizedFilePath')
import           Distribution.PackageDescription               (PackageDescription)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Ide.Plugin.Cabal.ExposedModules               (exposedBy,
                                                                packageModules)
import qualified Ide.Plugin.Cabal.Parse                        as Parse
import           System.Directory                              (createDirectoryIfMissing)
import           System.FilePath                               ((</>))
import           Test.Hls

exposedModulesTests :: TestTree
exposedModulesTests = testGroup "exposedBy"
  [ exposureCase "a library's exposed module is public API"
      ["pkg/src"] [("pkg/pkg.cabal", libPkg ["src"] ["Foo"] [])]
      "pkg/src/Foo.hs" "Foo" True
  , exposureCase "a library's other-module is not public API"
      ["pkg/src"] [("pkg/pkg.cabal", libPkg ["src"] [] ["Foo"])]
      "pkg/src/Foo.hs" "Foo" False
  , exposureCase "absent hs-source-dirs means the package root"
      ["pkg"] [("pkg/pkg.cabal", libPkg [] ["Foo"] [])]
      "pkg/Foo.hs" "Foo" True
  , exposureCase "an out-of-tree source dir is resolved"
      ["pkgA", "shared"] [("pkgA/pkgA.cabal", libPkg ["../shared"] ["Foo"] [])]
      "shared/Foo.hs" "Foo" True
  , -- The ancestor's `hs-source-dirs: .` covers the nested package too, so
    -- ownership has to prefer the more deeply nested source dir.
    exposureCase "a root-dir package does not claim a nested package's module"
      ["sub/src"]
      [ ("outer.cabal", libPkg ["."] ["Foo"] [])
      , ("sub/sub.cabal", libPkg ["src"] [] ["Foo"])
      ]
      "sub/src/Foo.hs" "Foo" False
  , exposureCase "a test-suite module sharing a library module's name is not public API"
      ["pkg/src", "pkg/test"]
      [("pkg/pkg.cabal", unlines (libLines ["src"] ["Utils"] [] ++ testLines ["test"] ["Utils"]))]
      "pkg/test/Utils.hs" "Utils" False
  , exposureCase "a private sublibrary exposes nothing"
      ["pkg/internal"]
      [("pkg/pkg.cabal", unlines (libLines ["src"] [] [] ++ privateSubLibLines ["internal"] ["Secret"]))]
      "pkg/internal/Secret.hs" "Secret" False
  , testCase "a file no component owns defaults to exposed" $
      exposedBy "/nowhere/Foo.hs" "Foo" [] @?= True
  ]

-- | @dirs@ are created under a canonical temp dir, then each @(path, text)@ is
-- parsed as the cabal file at that path.
exposureCase
  :: TestName -> [FilePath] -> [(FilePath, String)] -> FilePath -> T.Text -> Bool -> TestTree
exposureCase name dirs cabals file modName want =
  testCase name $ withCanonicalTempDir $ \tmp -> do
    mapM_ (createDirectoryIfMissing True . (tmp </>)) dirs
    comps <- concat <$> traverse
      (\(fp, txt) -> packageModules (toNormalizedFilePath' (tmp </> fp)) (parsePkg txt))
      cabals
    let got = exposedBy (tmp </> file) modName comps
    got == want @? (file <> ": expected exposed=" <> show want <> ", got " <> show got)

parsePkg :: String -> PackageDescription
parsePkg text = case snd (Parse.parseCabalFileContents (encodeUtf8 (T.pack text))) of
  Right gpd -> flattenPackageDescription gpd
  Left err  -> error ("exposedBy test: could not parse cabal: " <> show err)

-- | A one-library package, the shape most cases need.
libPkg :: [String] -> [String] -> [String] -> String
libPkg srcDirs exposed other = unlines (libLines srcDirs exposed other)

libLines :: [String] -> [String] -> [String] -> [String]
libLines = stanza ["cabal-version: 2.4", "name: p", "version: 0.1", "library"]

testLines :: [String] -> [String] -> [String]
testLines srcDirs other =
  stanza ["test-suite spec", "  type: exitcode-stdio-1.0", "  main-is: Spec.hs"]
    srcDirs [] other

privateSubLibLines :: [String] -> [String] -> [String]
privateSubLibLines srcDirs exposed =
  stanza ["library internal", "  visibility: private"] srcDirs exposed []

-- | @header@ then the fields every stanza kind shares, omitting the empty ones.
stanza :: [String] -> [String] -> [String] -> [String] -> [String]
stanza header srcDirs exposed other =
  header
    ++ [ "  hs-source-dirs: " <> intercalate ", " srcDirs | not (null srcDirs) ]
    ++ [ "  exposed-modules: " <> intercalate ", " exposed | not (null exposed) ]
    ++ [ "  other-modules: " <> intercalate ", " other | not (null other) ]
    ++ [ "  build-depends: base"
       , "  default-language: Haskell2010"
       ]
