module Main (main) where

import           Control.Lens               ((^.))
import           Data.Either                (rights)
import           Data.List                  (sort)
import qualified Data.Text                  as T
import           Ide.Plugin.Export          (Log, descriptor)
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath            ((</>))
import           Test.Hls

plugin :: PluginTestDescriptor Log
plugin = mkPluginTestDescriptor descriptor "export"

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-export-plugin" </> "test" </> "testdata"

runExport :: (FilePath -> Session a) -> IO a
runExport act =
    runSessionWithTestConfig def
        { testDirLocation = Left testDataDir
        , testPluginDescriptor = plugin
        } act

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

containsAfter :: TextDocumentIdentifier -> [T.Text] -> Session ()
containsAfter doc expected = do
    contents <- documentContents doc
    liftIO $ any (`T.isInfixOf` contents) expected
        @? ("Expected one of " <> show expected <> " in:\n" <> T.unpack contents)

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

    , testGroup "Add: negative cases"
        [ testCase "no action on implicit module" $ runExport $ \_dir -> do
            doc <- openDoc "Implicit.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 3 0)

        , testCase "no action when cursor on RHS" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 6 6)  -- col 6 is on the `2` of `bar = 2`
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
    ]
