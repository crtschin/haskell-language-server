module Main (main) where

import           Control.Lens               ((^.))
import           Data.Either                (rights)
import           Data.Foldable              (find)
import           Data.List                  (sort, tails)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Development.IDE.Test       (referenceReady)
import           Ide.Plugin.Export          (descriptor)
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath            (equalFilePath, joinPath,
                                             splitDirectories, (</>))
import           Test.Hls
import           Test.Hls.FileSystem        (directCradle, file,
                                             mkVirtualFileTree, text)

plugin :: PluginTestDescriptor ()
plugin = mkPluginTestDescriptor' descriptor "export"

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-export-plugin" </> "test" </> "testdata"

runExport :: (FilePath -> Session a) -> IO a
runExport act =
    runSessionWithTestConfig def
        { testDirLocation = Left testDataDir
        , testPluginDescriptor = plugin
        } act

testDataDirExposed :: FilePath
testDataDirExposed = "plugins" </> "hls-export-plugin" </> "test" </> "testdata-exposed"

runExportResolveExposed :: (FilePath -> Session a) -> IO a
runExportResolveExposed act =
    runSessionWithTestConfig def
        { testDirLocation = Left testDataDirExposed
        , testPluginDescriptor = plugin
        , testConfigCaps = codeActionResolveCaps
        } act

-- | A project materialised in a temp dir with no enclosing @.cabal@ file, so the
-- package's exposed modules cannot be determined.
runExportNoCabal :: (FilePath -> Session a) -> IO a
runExportNoCabal act =
    runSessionWithTestConfig def
        { testDirLocation = Right $ mkVirtualFileTree testDataDir
            [ directCradle ["NoCabal"]
            , file "NoCabal.hs" $ text $ T.unlines
                [ "module NoCabal where"
                , ""
                , "noCabalValue :: Int"
                , "noCabalValue = 1"
                ]
            ]
        , testPluginDescriptor = plugin
        , testConfigCaps = codeActionResolveCaps
        } act

waitForIndex :: FilePath -> Session ()
waitForIndex fp1 = skipManyTill anyMessage $ () <$ referenceReady suffixMatch
  where
    suffixMatch fp2 =
      any (equalFilePath fp1 . joinPath) (tails (splitDirectories fp2))

codeActionTitles :: TextDocumentIdentifier -> Range -> Session [T.Text]
codeActionTitles doc range =
    sort . map (^. L.title) . rights . map toEither
        <$> getCodeActions doc range

executeByPrefix :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
executeByPrefix prefix doc range = do
    actions <- rights . map toEither <$> getCodeActions doc range
    case filter (\ca -> prefix `T.isPrefixOf` (ca ^. L.title)) actions of
        (ca:_) -> executeCodeAction ca
        []     -> liftIO $ assertFailure (T.unpack prefix <> "...` action not offered")

executeExportAction, executeRemoveAction :: TextDocumentIdentifier -> Range -> Session ()
executeExportAction = executeByPrefix "Export `"
executeRemoveAction = executeByPrefix "Unexport `"

noActionWithPrefix :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
noActionWithPrefix prefix doc range = do
    titles <- codeActionTitles doc range
    liftIO $ not (any (prefix `T.isPrefixOf`) titles)
        @? ("Did not expect " <> T.unpack prefix <> " action; saw: " <> show titles)

noExportOffered, noRemoveOffered :: TextDocumentIdentifier -> Range -> Session ()
noExportOffered = noActionWithPrefix "Export `"
noRemoveOffered = noActionWithPrefix "Unexport `"

-- | Fail unless some variant is an infix of the text. The message dumps it.
assertAnyInfix :: T.Text -> [T.Text] -> Assertion
assertAnyInfix hay variants =
    any (`T.isInfixOf` hay) variants
        @? ("Expected one of " <> show variants <> " in:\n" <> T.unpack hay)

-- | Fail if any needle is an infix of the text.
assertNoneInfix :: T.Text -> [T.Text] -> Assertion
assertNoneInfix hay needles =
    not (any (`T.isInfixOf` hay) needles)
        @? ("Expected none of " <> show needles <> " in:\n" <> T.unpack hay)

containsAfter :: TextDocumentIdentifier -> [T.Text] -> Session ()
containsAfter doc expected = documentContents doc >>= liftIO . (`assertAnyInfix` expected)

-- | Assert a code action with this exact title is (or is not) offered.
assertOffered, assertNotOffered :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
assertOffered    = titleOffered True
assertNotOffered = titleOffered False

titleOffered :: Bool -> T.Text -> TextDocumentIdentifier -> Range -> Session ()
titleOffered want title doc range = do
    titles <- codeActionTitles doc range
    liftIO $ (title `elem` titles) == want
        @? (T.unpack title <> ": expected offered=" <> show want <> ", saw: " <> show titles)

rangeAt :: UInt -> UInt -> Range
rangeAt l c = Range (Position l c) (Position l c)

main :: IO ()
main = defaultTestRunner $ testGroup "Export"
    [ testGroup "Add: value bindings"
        [ testCase "add value to export list" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 6 0)
            containsAfter doc ["module AddExport (foo, Bar, bar)"]

        , testCase "no action when value already exported" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 3 0)  -- on `foo`

        , testCase "append follows a multi-line leading-comma list" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportMultiline.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 11 0)  -- on `baz`
            containsAfter doc ["  , baz\n  ) where"]
        ]

    , testGroup "Add: type declarations"
        [ testCase "add bare type as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 9 5)  -- on `Baz` type name
            containsAfter doc ["Baz(..)", "Baz (..)"]
        ]

    , testGroup "Add: constructors"
        [ testCase "constructor with no parent entry appends T (C)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 9 12)  -- on `Baz1`, no Baz entry yet
            containsAfter doc ["Baz (Baz1)", "Baz(Baz1)"]

        , testCase "constructor under bare-type parent promotes to T(C)" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 3 11)  -- on `Bar1`, Bar is IEThingAbs
            containsAfter doc ["Bar (Bar1)", "Bar(Bar1)"]

        , testCase "constructor merges into existing IEThingWith parent" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 2 18)  -- on `Foo2`, Foo has [Foo1]
            containsAfter doc ["Foo (Foo1, Foo2)", "Foo(Foo1, Foo2)"]

        , testCase "constructor already in IEThingWith children suppresses action" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 2 11)  -- on `Foo1`, already child of Foo(Foo1)

        , testCase "constructor under IEThingAll T(..) suppresses action" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 4 11)  -- on `Baz1`, Baz(..) covers it

        , testCase "constructor exported standalone suppresses action" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 5 11)  -- on `Qux1`, Qux1 standalone in list
        ]

    , testGroup "Add: type classes"
        [ testCase "add class as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddClass.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 8 6)  -- on `Baz` class name
            containsAfter doc ["module AddClass (Foo (..), Bar, Baz (..))"]

        , testCase "no add action when class exported as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddClass.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 2 6)  -- on `Foo`, exported as Foo (..)

        , testCase "no add action when class exported as bare T" $ runExport $ \_dir -> do
            doc <- openDoc "AddClass.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 5 6)  -- on `Bar`, exported as bare

        , testCase "no add action on class method" $ runExport $ \_dir -> do
            doc <- openDoc "AddClass.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 9 2)  -- on `baz1` inside `class Baz a where`
        ]

    , testGroup "Add: layout variants"
        [ testCase "add to an empty export list" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportEmpty.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 2 0)  -- on `foo`
            containsAfter doc ["module AddExportEmpty (foo) where"]

        , testCase "append after a trailing comma" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTrailingComma.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 7 0)  -- on `bar`
            containsAfter doc ["( foo, bar"]

        , testCase "preserve a haddock comment between items" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportComment.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 16 0)  -- on `quux`
            containsAfter doc ["  -- * For testing\n  , baz\n  , quux\n  ) where"]
        ]

    , testGroup "Add: declaration kinds"
        [ testCase "function operator is parenthesized" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 8 1)  -- on `(<|)`
            containsAfter doc ["(placeholder, (<|))"]

        , testCase "infix function exports bare name" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 11 3)  -- on `f`
            containsAfter doc ["(placeholder, f)"]

        , testCase "newtype exports as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 13 8)  -- on `NT`
            containsAfter doc ["placeholder, NT(..)", "placeholder, NT (..)"]

        , testCase "type synonym exports bare" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 15 5)  -- on `Syn`
            containsAfter doc ["(placeholder, Syn)"]

        , testCase "type family exports bare" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 17 12)  -- on `TF`
            containsAfter doc ["(placeholder, TF)"]

        , testCase "pattern synonym gets a pattern prefix" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 20 9)  -- on `Pat`
            containsAfter doc ["(placeholder, pattern Pat)"]

        , testCase "data operator gets type keyword and (..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 22 7)  -- on `(:<)`
            containsAfter doc ["placeholder, type (:<)(..)", "placeholder, type (:<) (..)"]
        ]

    , testGroup "Add: type-level operators"
        [ testCase "type synonym operator has no type keyword" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 8 7)  -- on `(:<>)`
            containsAfter doc ["(placeholder, (:<>))"]

        , testCase "type family operator gets type keyword" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 10 14)  -- on `(:+:)`
            containsAfter doc ["(placeholder, type (:+:))"]

        , testCase "typeclass operator gets type keyword and (..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 12 8)  -- on `(:*:)`
            containsAfter doc ["placeholder, type (:*:)(..)", "placeholder, type (:*:) (..)"]

        , testCase "newtype operator gets type keyword and (..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 14 10)  -- on `(:->)`
            containsAfter doc ["placeholder, type (:->)(..)", "placeholder, type (:->) (..)"]

        , testCase "pattern synonym operator is parenthesized" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 16 11)  -- on `(:++)`
            containsAfter doc ["(placeholder, pattern (:++))"]
        ]

    , testGroup "Add: negative cases"
        [ testCase "no action on implicit module" $ runExport $ \_dir -> do
            doc <- openDoc "Implicit.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 3 0)

        , testCase "no action when cursor on RHS" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 6 6)  -- col 6 is on the `2` of `bar = 2`

        , testCase "no action on a where-bound name" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportNegatives.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 7 8)  -- on `whereBound`

        , testCase "no action on a record field" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportNegatives.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 9 18)  -- on `recField`
        ]

    , testGroup "Export fixes the unused-binding warning"
        [ testCase "Export action attaches the -Wunused-top-binds diagnostic" $ runExport $ \_dir -> do
            doc <- openDoc "ExportUnusedFix.hs" "haskell"
            waitForKickDone
            actions <- rights . map toEither <$> getCodeActions doc (rangeAt 6 0)  -- on `unused`
            case filter ((== "Export `unused`") . (^. L.title)) actions of
                (ca:_) -> liftIO $ not (null (fromMaybe [] (ca ^. L.diagnostics)))
                            @? "Export action should carry the unused-binding diagnostic"
                []     -> liftIO $ assertFailure $
                            "Export `unused` not offered; saw: " <> show (map (^. L.title) actions)
        ]

    , testGroup "Remove: value bindings"
        [ testCase "remove first value (foo)" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveExport.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 3 0)
            containsAfter doc ["module RemoveExport (Bar, Baz (Baz1))"]

        , testCase "no remove action when value not in export list" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            noRemoveOffered doc (rangeAt 6 0)  -- `bar` not exported

        , testCase "remove first item of a multi-line list keeps own-line layout" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveFirstMultiline.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 6 0)  -- on `foo`, the first export
            containsAfter doc ["( bar\n  , baz\n  ) where"]
        ]

    , testGroup "Remove: type declarations"
        [ testCase "remove bare type (middle item)" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveExport.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 5 5)  -- on `Bar`
            containsAfter doc ["module RemoveExport (foo, Baz (Baz1))"]

        , testCase "remove IEThingWith type removes whole entry" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveCtor.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 2 5)  -- on `Foo` type
            containsAfter doc ["module RemoveCtor (Bar (..), Baz1)"]

        , testCase "remove IEThingAll type removes whole entry" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveCtor.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 3 5)  -- on `Bar` type with (..)
            containsAfter doc ["module RemoveCtor (Foo (Foo1, Foo2), Baz1)"]
        ]

    , testGroup "Remove: constructors"
        [ testCase "remove sole constructor downgrades to bare type" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveExport.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 6 11)  -- on `Baz1` in Baz(Baz1)
            containsAfter doc ["module RemoveExport (foo, Bar, Baz)"]

        , testCase "remove first constructor of T(C1, C2) yields T (C2)" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveCtor.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 2 11)  -- on `Foo1` in Foo(Foo1, Foo2)
            containsAfter doc ["Foo (Foo2)", "Foo(Foo2)"]

        , testCase "remove second constructor of T(C1, C2) yields T (C1)" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveCtor.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 2 18)  -- on `Foo2` in Foo(Foo1, Foo2)
            containsAfter doc ["Foo (Foo1)", "Foo(Foo1)"]

        , testCase "remove standalone-exported constructor" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveCtor.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 4 11)  -- on `Baz1` standalone
            containsAfter doc ["module RemoveCtor (Foo (Foo1, Foo2), Bar (..))"]

        , testCase "constructor under IEThingAll suppresses remove action" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveCtor.hs" "haskell"
            waitForKickDone
            noRemoveOffered doc (rangeAt 3 11)  -- on `Bar1`, only Bar(..) in list

        , testCase "constructor not in export list suppresses remove action" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveCtor.hs" "haskell"
            waitForKickDone
            noRemoveOffered doc (rangeAt 2 25)  -- on `Foo3`, not in any entry
        ]

    , testGroup "Remove: type classes"
        [ testCase "remove class exported as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveClass.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 2 6)  -- on `Foo`
            containsAfter doc ["module RemoveClass (Bar, Baz (baz1))"]

        , testCase "remove class exported as bare T" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveClass.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 5 6)  -- on `Bar`
            containsAfter doc ["module RemoveClass (Foo (..), Baz (baz1))"]

        , testCase "remove class exported as T(method)" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveClass.hs" "haskell"
            waitForKickDone
            executeRemoveAction doc (rangeAt 8 6)  -- on `Baz`
            containsAfter doc ["module RemoveClass (Foo (..), Bar)"]

        , testCase "no remove action when class not in export list" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveClass.hs" "haskell"
            waitForKickDone
            noRemoveOffered doc (rangeAt 12 6)  -- on `Qux`, not exported

        , testCase "no remove action on class method" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveClass.hs" "haskell"
            waitForKickDone
            noRemoveOffered doc (rangeAt 9 2)  -- on `baz1` inside `class Baz a where`
        ]

    , testGroup "Remove: negative cases"
        [ testCase "no remove action on implicit module" $ runExport $ \_dir -> do
            doc <- openDoc "Implicit.hs" "haskell"
            waitForKickDone
            noRemoveOffered doc (rangeAt 3 0)

        , testCase "no remove action when cursor on RHS" $ runExport $ \_dir -> do
            doc <- openDoc "RemoveExport.hs" "haskell"
            waitForKickDone
            noRemoveOffered doc (rangeAt 3 6)  -- on the `1` of `foo = 1`
        ]

    , testGroup "Export explicitly"
        [ testCase "implicit module: exports only externally-referenced names" $ runExportResolveExposed $ \_dir -> do
            _consumer <- openDoc "UsesMakeExplicitUsed.hs" "haskell"
            target    <- openDoc "MakeExplicitUsed.hs" "haskell"
            waitForIndex "UsesMakeExplicitUsed.hs"
            header <- exportHeaderAfter "Export explicitly" target
            liftIO $ do
                assertAnyInfix  header ["usedByOther, T (..)", "T (..), usedByOther"]
                assertNoneInfix header [",)", "usedOnlyInternally", "unusedEntirely"]

        , testCase "explicit module: refines list to externally-referenced names" $ runExportResolveExposed $ \_dir -> do
            _consumer <- openDoc "UsesRefineExports.hs" "haskell"
            target    <- openDoc "RefineExports.hs" "haskell"
            waitForIndex "UsesRefineExports.hs"
            header <- exportHeaderAfter "Export explicitly" target
            liftIO $ do
                assertAnyInfix  header ["used, T (..)", "T (..), used"]
                assertNoneInfix header [",)", "unused", "UnusedT"]

        , testCase "explicit module: preserves multi-line layout when trimming" $ runExportResolveExposed $ \_dir -> do
            _consumer <- openDoc "UsesMultilineRefine.hs" "haskell"
            target    <- openDoc "MultilineRefine.hs" "haskell"
            waitForIndex "UsesMultilineRefine.hs"
            runActionWithTitle "Export explicitly" target (rangeAt 0 7)
            contents <- documentContents target
            liftIO $ do
                assertAnyInfix  contents ["  ( used\n  , T (..)\n  ) where"]
                assertNoneInfix (fst (T.breakOn "where" contents)) ["unused"]

        , testCase "no action when cursor is off the module header" $ runExportResolveExposed $ \_dir -> do
            doc <- openDoc "MakeExplicitUsed.hs" "haskell"
            waitForKickDone
            assertNotOffered "Export explicitly" doc (rangeAt 3 0)  -- on `usedByOther`

        , testCase "not offered on a module exposed by the library" $ runExportResolveExposed $ \_dir -> do
            -- ExposedApi is exposed by the library, so its exports are public API and the action must not appear.
            target <- openDoc "ExposedApi.hs" "haskell"
            waitForKickDone
            assertNotOffered "Export explicitly" target (rangeAt 0 7)

        , testCase "still offered on a non-exposed module" $ runExportResolveExposed $ \_dir -> do
            -- InternalApi is not exposed by the library, so the trim action still applies.
            target <- openDoc "InternalApi.hs" "haskell"
            waitForKickDone
            assertOffered "Export explicitly" target (rangeAt 0 7)

        , testCase "not offered when no cabal file can be found" $ runExportNoCabal $ \_dir -> do
            -- Without a cabal file the public API is unknown, so the action is withheld.
            target <- openDoc "NoCabal.hs" "haskell"
            waitForKickDone
            assertNotOffered "Export explicitly" target (rangeAt 0 7)
        ]

    , testGroup "Export all symbols"
        [ testCase "implicit module: lists every top-level symbol with its flavor" $ runExportResolveExposed $ \_dir -> do
            target <- openDoc "ExportAll.hs" "haskell"
            waitForKickDone
            header <- exportHeaderAfter "Export all symbols" target
            liftIO $ do
                assertAnyInfix  header ["value"]
                assertAnyInfix  header ["Rec (..)", "Rec(..)"]
                assertAnyInfix  header ["NT (..)", "NT(..)"]
                assertAnyInfix  header ["Cls (..)", "Cls(..)"]
                assertAnyInfix  header ["Syn"]
                -- record fields and class methods fold into (..), never listed standalone
                assertNoneInfix header [",)", "field", "method"]

        , testCase "partial explicit list: appends the missing symbols, keeps existing entries and re-exports" $ runExportResolveExposed $ \_dir -> do
            target <- openDoc "ExpandExports.hs" "haskell"
            waitForKickDone
            header <- exportHeaderAfter "Export all symbols" target
            liftIO $ do
                assertAnyInfix  header ["alreadyListed"]            -- pre-existing local entry kept
                assertAnyInfix  header ["notYetListed"]             -- missing local appended
                assertAnyInfix  header ["Extra (..)", "Extra(..)"]
                assertAnyInfix  header ["sort"]                     -- re-export of an import survives
                assertNoneInfix header [",)"]
                T.count "sort" header == 1
                    @? ("Re-export should not be duplicated:\n" <> T.unpack header)

        , testCase "offered regardless of whether the module is exposed" $ runExportResolveExposed $ \_dir -> do
            -- Listing all exports is non-destructive, so unlike the trim action it
            -- is not withheld on a library-exposed module.
            target <- openDoc "ExposedApi.hs" "haskell"
            waitForKickDone
            assertOffered "Export all symbols" target (rangeAt 0 7)

        , testCase "not offered when every symbol is already exported" $ runExportResolveExposed $ \_dir -> do
            target <- openDoc "Complete.hs" "haskell"
            waitForKickDone
            assertNotOffered "Export all symbols" target (rangeAt 0 7)

        , testCase "not offered off the module header" $ runExportResolveExposed $ \_dir -> do
            target <- openDoc "ExportAll.hs" "haskell"
            waitForKickDone
            assertNotOffered "Export all symbols" target (rangeAt 2 0)  -- on `value`
        ]
    ]

runActionWithTitle :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
runActionWithTitle title doc range = do
    actions <- rights . map toEither <$> getCodeActions doc range
    case find ((== title) . (^. L.title)) actions of
        Just ca -> do
            resolved <- resolveCodeAction ca
            executeCodeAction resolved
        Nothing -> liftIO $ assertFailure $
            T.unpack title <> " action not offered, saw: " <> show (map (^. L.title) actions)

exportHeader :: T.Text -> T.Text
exportHeader = T.takeWhile (/= '\n')

-- | Run the titled action on the module header and return the rewritten header line.
exportHeaderAfter :: T.Text -> TextDocumentIdentifier -> Session T.Text
exportHeaderAfter title doc = do
    runActionWithTitle title doc (rangeAt 0 7)
    exportHeader <$> documentContents doc
