module Main (main) where

import           Control.Lens               ((^.))
import           Data.Char                  (isSpace)
import           Data.Either                (rights)
import           Data.Foldable              (find)
import           Data.List                  (sort)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Ide.Plugin.Export          (descriptor)
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath            ((</>))
import           Test.Hls
import           Test.Hls.FileSystem        (copy, directProject,
                                             mkVirtualFileTree)

plugin :: PluginTestDescriptor ()
plugin = mkPluginTestDescriptor' descriptor "export"

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-export-plugin" </> "test" </> "testdata"

-- | Open the named module in its own temporary single-file project, so each
-- test compiles only the file it needs and cannot pick up signals from a
-- sibling module.
runExport :: FilePath -> (TextDocumentIdentifier -> Session a) -> IO a
runExport = runExportWith []

-- | Like 'runExport' but also copies the named extra files into the project,
-- e.g. a header a CPP @#include@ pulls in next to the module.
runExportWith :: [FilePath] -> FilePath -> (TextDocumentIdentifier -> Session a) -> IO a
runExportWith extra hsFile act =
    runSessionWithTestConfig def
        { testDirLocation = Right (mkVirtualFileTree testDataDir (directProject hsFile <> map copy extra))
        , testPluginDescriptor = plugin
        } $ \_dir -> do
            doc <- openDoc hsFile "haskell"
            waitForKickDone
            act doc

testDataDirExposed :: FilePath
testDataDirExposed = "plugins" </> "hls-export-plugin" </> "test" </> "testdata-exposed"

runExportResolveExposed :: (FilePath -> Session a) -> IO a
runExportResolveExposed act =
    runSessionWithTestConfig def
        { testDirLocation = Left testDataDirExposed
        , testPluginDescriptor = plugin
        , testConfigCaps = codeActionResolveCaps
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

-- | Assert a code action with this exact title is (or is not) offered.
assertOffered, assertNotOffered :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
assertOffered    = titleOffered True
assertNotOffered = titleOffered False

titleOffered :: Bool -> T.Text -> TextDocumentIdentifier -> Range -> Session ()
titleOffered want title doc range = do
    titles <- codeActionTitles doc range
    liftIO $ (title `elem` titles) == want
        @? (T.unpack title <> ": expected offered=" <> show want <> ", saw: " <> show titles)

containsAfter :: TextDocumentIdentifier -> [T.Text] -> Session ()
containsAfter doc expected = documentContents doc >>= liftIO . (`assertAnyInfix` expected)

-- | Fail unless every needle is an infix of the haystack. Used to assert that
-- CPP directives and conditional items survive an edit verbatim.
assertContainsAll :: T.Text -> [T.Text] -> Assertion
assertContainsAll hay = mapM_ $ \needle ->
    needle `T.isInfixOf` hay
        @? ("Expected " <> show needle <> " in:\n" <> T.unpack hay)

-- | Lines from the first @(@ through the @) where@ line (included, so an item on
-- the closing line still counts).
exportListRegion :: T.Text -> [T.Text]
exportListRegion txt =
    let afterOpen     = dropWhile (not . T.isInfixOf "(") (T.lines txt)
        (body, close) = break (T.isInfixOf ") where") afterOpen
    in body ++ take 1 close

-- | Does @name@ appear in the export list at CPP nesting level 0, i.e. not
-- guarded by any @#if@/@#ifdef@/@#ifndef@?
exportedUnconditionally :: T.Text -> T.Text -> Bool
exportedUnconditionally name txt = go (0 :: Int) (exportListRegion txt)
  where
    go _ [] = False
    go n (l:ls)
        | "#if"    `T.isPrefixOf` T.stripStart l = go (n + 1) ls
        | "#endif" `T.isPrefixOf` T.stripStart l = go (max 0 (n - 1)) ls
        | n == 0, name `T.isInfixOf` l           = True
        | otherwise                              = go n ls

-- | True when the export-list region carries no doubled or leading comma. A
-- trailing comma before @)@ is legal Haskell, so @,)@ is not flagged.
wellFormedExportList :: T.Text -> Bool
wellFormedExportList txt = not (any (`T.isInfixOf` compact) ["(,", ",,"])
  where
    compact = T.filter (not . isSpace) (T.unlines (exportListRegion txt))

-- | Run an action at the position, assert the result has a well-formed export
-- list, and return the resulting document text.
runAndCheck :: (TextDocumentIdentifier -> Range -> Session ()) -> TextDocumentIdentifier -> Range -> Session T.Text
runAndCheck act doc pos = do
    act doc pos
    txt <- documentContents doc
    liftIO $ wellFormedExportList txt
        @? ("malformed export list, got:\n" <> T.unpack txt)
    pure txt

exportAndCheck, removeAndCheck :: TextDocumentIdentifier -> Range -> Session T.Text
exportAndCheck = runAndCheck executeExportAction
removeAndCheck = runAndCheck executeRemoveAction

-- | Crudely re-run CPP for the macro EXAMPLE_FLAG over already-edited text,
-- keeping the branch the given definedness selects. Single level, just enough
-- to inspect the configuration the server did not parse.
preprocessExampleFlag :: Bool -> T.Text -> T.Text
preprocessExampleFlag defined = T.unlines . go Nothing . T.lines
  where
    -- Nothing outside any conditional. Just b inside one, emitting only when b.
    go _ [] = []
    go st (l:ls)
        | isDir "#ifdef"  = go (Just defined) ls
        | isDir "#ifndef" = go (Just (not defined)) ls
        | isDir "#else"   = go (fmap not st) ls
        | isDir "#endif"  = go Nothing ls
        | fromMaybe True st = l : go st ls
        | otherwise         = go st ls
      where isDir d = d `T.isPrefixOf` T.stripStart l

rangeAt :: UInt -> UInt -> Range
rangeAt l c = Range (Position l c) (Position l c)

-- | The CPP block the testdata guards with @#ifdef EXAMPLE_FLAG@. The flag is
-- never defined, so the branch is inactive and must survive an edit verbatim.
flagBlock :: [T.Text]
flagBlock = ["#ifdef EXAMPLE_FLAG", ", flagged", "#endif"]

-- | The new export of @name@ must sit at CPP nesting level 0, never in a branch.
assertExportedUnconditionally :: T.Text -> T.Text -> Assertion
assertExportedUnconditionally name txt =
    exportedUnconditionally name txt
        @? (T.unpack name <> " must be exported outside any CPP branch, got:\n" <> T.unpack txt)

-- | The 'flagBlock' survives verbatim and @name@ lands outside it.
assertFlaggedBlockKept :: T.Text -> T.Text -> Assertion
assertFlaggedBlockKept name txt =
    assertContainsAll txt flagBlock >> assertExportedUnconditionally name txt

-- | @name@ no longer appears anywhere in the export-list region.
assertNotInExportList :: T.Text -> T.Text -> Assertion
assertNotInExportList name txt =
    not (name `T.isInfixOf` T.unlines (exportListRegion txt))
        @? (T.unpack name <> " should be gone from the export list, got:\n" <> T.unpack txt)

-- | Both CPP configurations of the edited text keep a well-formed export list,
-- so a surgical delete never breaks the branch the server did not parse.
assertBothConfigsWellFormed :: T.Text -> Assertion
assertBothConfigsWellFormed txt = mapM_ check [True, False]
  where
    check defined =
        let cfg = preprocessExampleFlag defined txt
        in wellFormedExportList cfg
            @? ("malformed export list for EXAMPLE_FLAG=" <> show defined <> ":\n" <> T.unpack cfg)

-- | Export the binding at the position and assert the result contains one of
-- the @expected@ variants.
addCase :: TestName -> FilePath -> UInt -> UInt -> [T.Text] -> TestTree
addCase name file l c expected = testCase name $ runExport file $ \doc -> do
    executeExportAction doc (rangeAt l c)
    containsAfter doc expected

-- | Assert no export action is offered at the position.
noCase :: TestName -> FilePath -> UInt -> UInt -> TestTree
noCase name file l c = testCase name $ runExport file $ \doc ->
    noExportOffered doc (rangeAt l c)

-- | Export the binding at the position, assert the list is well-formed, then
-- run @check@ over the resulting document text.
exportCase :: TestName -> FilePath -> UInt -> UInt -> (T.Text -> Assertion) -> TestTree
exportCase name file l c check = testCase name $ runExport file $ \doc -> do
    txt <- exportAndCheck doc (rangeAt l c)
    liftIO (check txt)

main :: IO ()
main = defaultTestRunner $ testGroup "Export"
    [ testGroup "Add: value bindings"
        [ addCase "add value to export list" "AddExport.hs" 6 0
            ["module AddExport (foo, Bar, bar)"]
        , noCase "no action when value already exported" "AddExport.hs" 3 0  -- on `foo`
        , addCase "append follows a multi-line leading-comma list" "AddExportMultiline.hs" 11 0  -- on `baz`
            ["  , baz\n  ) where"]
        ]

    , testGroup "Add: type declarations"
        [ addCase "add bare type as T(..)" "AddExport.hs" 9 5  -- on `Baz` type name
            ["Baz(..)", "Baz (..)"]
        ]

    , testGroup "Add: constructors"
        [ addCase "constructor with no parent entry appends T (C)" "AddExport.hs" 9 12  -- on `Baz1`, no Baz entry yet
            ["Baz (Baz1)", "Baz(Baz1)"]
        , addCase "constructor under bare-type parent promotes to T(C)" "AddCtor.hs" 3 11  -- on `Bar1`, Bar is IEThingAbs
            ["Bar (Bar1)", "Bar(Bar1)"]
        , addCase "constructor merges into existing IEThingWith parent" "AddCtor.hs" 2 18  -- on `Foo2`, Foo has [Foo1]
            ["Foo (Foo1, Foo2)", "Foo(Foo1, Foo2)"]
        , noCase "constructor already in IEThingWith children suppresses action" "AddCtor.hs" 2 11  -- on `Foo1`, already child of Foo(Foo1)
        , noCase "constructor under IEThingAll T(..) suppresses action" "AddCtor.hs" 4 11  -- on `Baz1`, Baz(..) covers it
        , noCase "constructor exported standalone suppresses action" "AddCtor.hs" 5 11  -- on `Qux1`, Qux1 standalone in list
        ]

    , testGroup "Add: type classes"
        [ addCase "add class as T(..)" "AddClass.hs" 8 6  -- on `Baz` class name
            ["module AddClass (Foo (..), Bar, Baz (..))"]
        , noCase "no add action when class exported as T(..)" "AddClass.hs" 2 6  -- on `Foo`, exported as Foo (..)
        , noCase "no add action when class exported as bare T" "AddClass.hs" 5 6  -- on `Bar`, exported as bare
        , noCase "no add action on class method" "AddClass.hs" 9 2  -- on `baz1` inside `class Baz a where`
        ]

    , testGroup "Add: layout variants"
        [ addCase "add to an empty export list" "AddExportEmpty.hs" 2 0  -- on `foo`
            ["module AddExportEmpty (foo) where"]
        , addCase "append after a trailing comma" "AddExportTrailingComma.hs" 7 0  -- on `bar`
            ["( foo, bar"]
        , addCase "preserve a haddock comment between items" "AddExportComment.hs" 16 0  -- on `quux`
            ["  -- * For testing\n  , baz\n  , quux\n  ) where"]
        ]

    , testGroup "Add: declaration kinds"
        [ addCase "function operator is parenthesized" "AddExportKinds.hs" 8 1  -- on `(<|)`
            ["(placeholder, (<|))"]
        , addCase "infix function exports bare name" "AddExportKinds.hs" 11 3  -- on `f`
            ["(placeholder, f)"]
        , addCase "newtype exports as T(..)" "AddExportKinds.hs" 13 8  -- on `NT`
            ["placeholder, NT(..)", "placeholder, NT (..)"]
        , addCase "type synonym exports bare" "AddExportKinds.hs" 15 5  -- on `Syn`
            ["(placeholder, Syn)"]
        , addCase "type family exports bare" "AddExportKinds.hs" 17 12  -- on `TF`
            ["(placeholder, TF)"]
        , addCase "pattern synonym gets a pattern prefix" "AddExportKinds.hs" 20 9  -- on `Pat`
            ["(placeholder, pattern Pat)"]
        , addCase "data operator gets type keyword and (..)" "AddExportKinds.hs" 22 7  -- on `(:<)`
            ["placeholder, type (:<)(..)", "placeholder, type (:<) (..)"]
        ]

    , testGroup "Add: type-level operators"
        [ addCase "type synonym operator has no type keyword" "AddExportTypeOps.hs" 8 7  -- on `(:<>)`
            ["(placeholder, (:<>))"]
        , addCase "type family operator gets type keyword" "AddExportTypeOps.hs" 10 14  -- on `(:+:)`
            ["(placeholder, type (:+:))"]
        , addCase "typeclass operator gets type keyword and (..)" "AddExportTypeOps.hs" 12 8  -- on `(:*:)`
            ["placeholder, type (:*:)(..)", "placeholder, type (:*:) (..)"]
        , addCase "newtype operator gets type keyword and (..)" "AddExportTypeOps.hs" 14 10  -- on `(:->)`
            ["placeholder, type (:->)(..)", "placeholder, type (:->) (..)"]
        , addCase "pattern synonym operator is parenthesized" "AddExportTypeOps.hs" 16 11  -- on `(:++)`
            ["(placeholder, pattern (:++))"]
        ]

    , testGroup "Add: negative cases"
        [ noCase "no action on implicit module" "Implicit.hs" 3 0
        , noCase "no action when cursor on RHS" "AddExport.hs" 6 6  -- col 6 is on the `2` of `bar = 2`
        , noCase "no action on a where-bound name" "AddExportNegatives.hs" 7 8  -- on `whereBound`
        , noCase "no action on a record field" "AddExportNegatives.hs" 9 18  -- on `recField`
        ]

    , testGroup "Add: CPP in the export list"
        -- EXAMPLE_FLAG is never defined in the test project, so #ifdef branches
        -- are inactive and #ifndef branches are active. The edit must preserve
        -- every directive verbatim and place the new export outside any branch.
        [ exportCase "preserves a trailing #ifdef block" "CppExportTail.hs" 15 0  -- on `baz`
            (assertFlaggedBlockKept "baz")

        , exportCase "preserves a leading #ifndef block" "CppExportHead.hs" 12 0 $ \txt -> do  -- on `bar`
            -- the whole guarded block survives verbatim, not just stray substrings
            assertContainsAll txt ["#ifndef EXAMPLE_FLAG\n    foo\n#endif"]
            assertExportedUnconditionally "bar" txt

        , exportCase "preserves both #if/#else branches" "CppExportElse.hs" 20 0 $ \txt -> do  -- on `extra`
            assertContainsAll txt
                ["#ifdef EXAMPLE_FLAG", ", windows", "#else", ", posix", "#endif"]
            assertExportedUnconditionally "extra" txt

        , testCase "preserves an #include directive" $ runExportWith ["CppExportInclude.h"] "CppExportInclude.hs" $ \doc -> do
            txt <- exportAndCheck doc (rangeAt 13 0)  -- on `extra`
            liftIO $ do
                assertContainsAll txt ["#include \"CppExportInclude.h\"", "( extra, foo"]
                assertExportedUnconditionally "extra" txt

        , exportCase "appends a new T(C) beside a CPP block" "CppCtorAppend.hs" 11 11 $ \txt -> do  -- on `Baz1`, no Baz entry yet
            assertFlaggedBlockKept "Baz1" txt
            txt `assertAnyInfix` ["Baz (Baz1)", "Baz(Baz1)"]

        , exportCase "adds a separate entry beside an IEThingWith parent" "CppCtorExtend.hs" 8 18 $ \txt -> do  -- on `Foo2`, Foo has [Foo1]
            assertFlaggedBlockKept "Foo2" txt
            assertContainsAll txt ["Foo(Foo1)"]
            txt `assertAnyInfix` ["Foo (Foo2)", "Foo(Foo2)"]

        , exportCase "adds a separate entry without a double comma" "CppCtorMid.hs" 9 18 $ \txt -> do  -- on `Foo2`, Foo(Foo1) precedes `, bar`
            assertContainsAll txt (flagBlock <> [", bar", "Foo(Foo1)"])
            txt `assertAnyInfix` ["Foo (Foo2)", "Foo(Foo2)"]

        , exportCase "adds a separate entry beside a bare-type parent" "CppCtorUpgrade.hs" 8 11 $ \txt -> do  -- on `Bar1`, Bar is IEThingAbs
            assertFlaggedBlockKept "Bar1" txt
            txt `assertAnyInfix` ["Bar (Bar1)", "Bar(Bar1)"]

        , exportCase "exports an operator beside a CPP block" "CppExportKinds.hs" 12 1  -- on `(<|)`
            (assertFlaggedBlockKept "(<|)")

        , exportCase "exports a pattern synonym beside a CPP block" "CppExportKinds.hs" 16 8  -- on `Zero`
            (assertFlaggedBlockKept "pattern Zero")

        , exportCase "adds a separate entry beside a directive inside a constructor list" "CppCtorIntra.hs" 9 24 $ \txt -> do  -- on `Foo2`
            -- the #ifdef sits inside Foo(...), where an in-place merge would erase it
            assertContainsAll txt ["#ifdef EXAMPLE_FLAG\n      , Bar\n#endif", "Foo(Foo1"]
            txt `assertAnyInfix` ["Foo (Foo2)", "Foo(Foo2)"]

        , exportCase "front-inserts even when the close paren shares a line" "CppExportParenShared.hs" 18 0 $ \txt -> do  -- on `baz`
            assertContainsAll txt (flagBlock <> [", bar )"])
            assertExportedUnconditionally "baz" txt

        , exportCase "no double comma when the last item already has a trailing comma" "CppExportTrailingComma.hs" 15 0 $ \txt -> do  -- on `baz`
            -- a doubled `,,` would be caught by exportAndCheck's well-formedness check
            assertContainsAll txt ["#ifdef EXAMPLE_FLAG", "flagged,", "#endif"]
            assertExportedUnconditionally "baz" txt

        , exportCase "edit stays valid in the unparsed CPP branch" "CppExportOtherBranch.hs" 12 0 $ \txt -> do  -- on `bar`, the only item is in the other branch
            -- the single item lives under #ifndef, so it is the whole parsed list.
            -- The front-insert plus trailing comma stays valid when the flag flips
            -- and that item disappears.
            let otherBranch = preprocessExampleFlag True txt
            wellFormedExportList otherBranch
                @? ("edit breaks the EXAMPLE_FLAG-defined configuration:\n" <> T.unpack otherBranch)
        ]

    , testGroup "Export fixes the unused-binding warning"
        [ knownBrokenForGhcVersions [GHC96]
            "TcRnUnusedName provenance is unstructured before GHC 9.8 (GHC #20115)" $
          testCase "Export action attaches the -Wunused-top-binds diagnostic" $ runExport "ExportUnusedFix.hs" $ \doc -> do
            actions <- rights . map toEither <$> getCodeActions doc (rangeAt 6 0)  -- on `unused`
            case filter ((== "Export `unused`") . (^. L.title)) actions of
                (ca:_) -> liftIO $ not (null (fromMaybe [] (ca ^. L.diagnostics)))
                            @? "Export action should carry the unused-binding diagnostic"
                []     -> liftIO $ assertFailure $
                            "Export `unused` not offered; saw: " <> show (map (^. L.title) actions)
        ]

    , testGroup "Remove: value bindings"
        [ testCase "remove first value (foo)" $ runExport "RemoveExport.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 3 0)
            containsAfter doc ["module RemoveExport (Bar, Baz (Baz1))"]

        , testCase "no remove action when value not in export list" $ runExport "AddExport.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 6 0)  -- `bar` not exported

        , testCase "remove first item of a multi-line list keeps own-line layout" $ runExport "RemoveFirstMultiline.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 6 0)  -- on `foo`, the first export
            containsAfter doc ["( bar\n  , baz\n  ) where"]
        ]

    , testGroup "Remove: type declarations"
        [ testCase "remove bare type (middle item)" $ runExport "RemoveExport.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 5 5)  -- on `Bar`
            containsAfter doc ["module RemoveExport (foo, Baz (Baz1))"]

        , testCase "remove IEThingWith type removes whole entry" $ runExport "RemoveCtor.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 2 5)  -- on `Foo` type
            containsAfter doc ["module RemoveCtor (Bar (..), Baz1)"]

        , testCase "remove IEThingAll type removes whole entry" $ runExport "RemoveCtor.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 3 5)  -- on `Bar` type with (..)
            containsAfter doc ["module RemoveCtor (Foo (Foo1, Foo2), Baz1)"]
        ]

    , testGroup "Remove: constructors"
        [ testCase "remove sole constructor downgrades to bare type" $ runExport "RemoveExport.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 6 11)  -- on `Baz1` in Baz(Baz1)
            containsAfter doc ["module RemoveExport (foo, Bar, Baz)"]

        , testCase "remove first constructor of T(C1, C2) yields T (C2)" $ runExport "RemoveCtor.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 2 11)  -- on `Foo1` in Foo(Foo1, Foo2)
            containsAfter doc ["Foo (Foo2)", "Foo(Foo2)"]

        , testCase "remove second constructor of T(C1, C2) yields T (C1)" $ runExport "RemoveCtor.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 2 18)  -- on `Foo2` in Foo(Foo1, Foo2)
            containsAfter doc ["Foo (Foo1)", "Foo(Foo1)"]

        , testCase "remove standalone-exported constructor" $ runExport "RemoveCtor.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 4 11)  -- on `Baz1` standalone
            containsAfter doc ["module RemoveCtor (Foo (Foo1, Foo2), Bar (..))"]

        , testCase "constructor under IEThingAll suppresses remove action" $ runExport "RemoveCtor.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 3 11)  -- on `Bar1`, only Bar(..) in list

        , testCase "constructor not in export list suppresses remove action" $ runExport "RemoveCtor.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 2 25)  -- on `Foo3`, not in any entry

        , testCase "constructor sharing its type's name does not unexport the type" $ runExport "RemoveCtorNameClash.hs" $ \doc ->
            -- `data Bar = Bar`; the constructor is not exported (only the abstract
            -- type is), so unexport must not fall back to removing the type entry.
            noRemoveOffered doc (rangeAt 5 11)  -- on the constructor `Bar`

        , testCase "unexporting the type still works when a constructor shares its name" $ runExport "RemoveCtorNameClash.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 5 5)  -- on the type `Bar`
            containsAfter doc ["module RemoveCtorNameClash (foo)"]

        , testCase "downgrading an operator type keeps the type keyword" $ runExport "RemoveCtorOp.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 4 15)  -- on `Op`, the sole constructor of (:+:)
            containsAfter doc ["(type (:+:)) where", "(type (:+:) ) where"]
        ]

    , testGroup "Remove: type classes"
        [ testCase "remove class exported as T(..)" $ runExport "RemoveClass.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 2 6)  -- on `Foo`
            containsAfter doc ["module RemoveClass (Bar, Baz (baz1))"]

        , testCase "remove class exported as bare T" $ runExport "RemoveClass.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 5 6)  -- on `Bar`
            containsAfter doc ["module RemoveClass (Foo (..), Baz (baz1))"]

        , testCase "remove class exported as T(method)" $ runExport "RemoveClass.hs" $ \doc -> do
            executeRemoveAction doc (rangeAt 8 6)  -- on `Baz`
            containsAfter doc ["module RemoveClass (Foo (..), Bar)"]

        , testCase "no remove action when class not in export list" $ runExport "RemoveClass.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 12 6)  -- on `Qux`, not exported

        , testCase "no remove action on class method" $ runExport "RemoveClass.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 9 2)  -- on `baz1` inside `class Baz a where`
        ]

    , testGroup "Remove: negative cases"
        [ testCase "no remove action on implicit module" $ runExport "Implicit.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 3 0)

        , testCase "no remove action when cursor on RHS" $ runExport "RemoveExport.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 3 6)  -- on the `1` of `foo = 1`
        ]

    , testGroup "Remove: CPP in the export list"
        -- EXAMPLE_FLAG is never defined, so #ifdef branches are inactive. A
        -- surgical unexport must keep every directive verbatim and stay valid in
        -- both configurations; it is withheld when the symbol's only active
        -- neighbour sits across a directive (no directive-free side to delete).
        [ testCase "unexports a first item with a clean following sibling" $ runExport "CppUnexportClean.hs" $ \doc -> do
            txt <- removeAndCheck doc (rangeAt 10 0)  -- on `foo`
            liftIO $ do
                assertContainsAll txt (flagBlock <> ["( bar"])
                assertNotInExportList "foo" txt
                assertBothConfigsWellFormed txt

        , testCase "unexports a last item with a clean preceding sibling" $ runExport "CppUnexportClean.hs" $ \doc -> do
            txt <- removeAndCheck doc (rangeAt 13 0)  -- on `bar`
            liftIO $ do
                assertContainsAll txt (flagBlock <> ["( foo"])
                assertNotInExportList "bar" txt
                assertBothConfigsWellFormed txt

        , testCase "no unexport for the sole active item" $ runExport "CppExportTail.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 9 0)  -- on `foo`, flagged is conditional

        , testCase "no unexport when a directive wedges the only neighbour (last)" $ runExport "CppUnexportWedged.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 13 0)  -- on `bar`, separated from `foo` by the block

        , testCase "no unexport when a directive wedges the only neighbour (first)" $ runExport "CppUnexportWedged.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 10 0)  -- on `foo`, separated from `bar` by the block

        , testCase "unexports a constructor child beside a directive" $ runExport "CppCtorUnexport.hs" $ \doc -> do
            txt <- removeAndCheck doc (rangeAt 17 18)  -- on `Foo2`
            liftIO $ do
                assertContainsAll txt ["#ifdef EXAMPLE_FLAG\n      , Foo3\n#endif"]
                assertNotInExportList "Foo2" txt
                assertBothConfigsWellFormed txt

        , testCase "no unexport for the only active constructor child" $ runExport "CppCtorUnexport.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 18 11)  -- on `Bar1`, sole active child of Bar(...)

        , testCase "no unexport for a constructor only in an inactive branch" $ runExport "CppCtorUnexport.hs" $ \doc ->
            noRemoveOffered doc (rangeAt 17 25)  -- on `Foo3`, under #ifdef
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

        , testCase "withheld when the export list contains a CPP directive" $ runExportResolveExposed $ \_dir -> do
            -- Reprinting the list would drop the #ifdef the parser stripped, so the
            -- additive expand is withheld (mirrors the remove actions' CPP refusal).
            target <- openDoc "CppExpand.hs" "haskell"
            waitForKickDone
            assertNotOffered "Export all symbols" target (rangeAt 1 7)  -- on the module name

        , testCase "still offered when directives are outside the export list" $ runExportResolveExposed $ \_dir -> do
            target <- openDoc "CppExpandSafe.hs" "haskell"
            waitForKickDone
            assertOffered "Export all symbols" target (rangeAt 1 7)  -- on the module name
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
