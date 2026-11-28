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

codeActionTitles :: TextDocumentIdentifier -> Range -> Session [T.Text]
codeActionTitles doc range =
    sort . map (^. L.title) . rights . map toEither
        <$> getCodeActions doc range

executeByPrefix :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
executeByPrefix prefix doc range =
    findAction (prefix `T.isPrefixOf`) doc range >>= executeCodeAction

executeExportAction, executeRemoveAction, executeExportAllAction :: TextDocumentIdentifier -> Range -> Session ()
executeExportAction = executeByPrefix "Export `"
executeRemoveAction = executeByPrefix "Unexport `"
executeExportAllAction = executeByPrefix "Export all symbols"

noActionWithPrefix :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
noActionWithPrefix prefix doc range = do
    titles <- codeActionTitles doc range
    liftIO $ not (any (prefix `T.isPrefixOf`) titles)
        @? ("Did not expect " <> T.unpack prefix <> " action, saw: " <> show titles)

noExportOffered, noRemoveOffered, noExportAllOffered :: TextDocumentIdentifier -> Range -> Session ()
noExportOffered = noActionWithPrefix "Export `"
noRemoveOffered = noActionWithPrefix "Unexport `"
noExportAllOffered = noActionWithPrefix "Export all symbols"

-- | Fail unless some variant is an infix of the text. The message dumps it.
assertAnyInfix :: T.Text -> [T.Text] -> Assertion
assertAnyInfix hay variants =
    any (`T.isInfixOf` hay) variants
        @? ("Expected one of " <> show variants <> " in:\n" <> T.unpack hay)

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

-- | Run an action at the position, check the export list is well formed, return the new text.
runAndCheck :: (TextDocumentIdentifier -> Range -> Session ()) -> TextDocumentIdentifier -> Range -> Session T.Text
runAndCheck act doc pos = do
    act doc pos
    txt <- documentContents doc
    liftIO $ wellFormedExportList txt
        @? ("malformed export list, got:\n" <> T.unpack txt)
    pure txt

exportAndCheck :: TextDocumentIdentifier -> Range -> Session T.Text
exportAndCheck = runAndCheck executeExportAction

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

-- | Run @exec@ at the position, assert the resulting export list is well-formed,
-- then assert the document contains one of the @expected@ variants.
execCase :: (TextDocumentIdentifier -> Range -> Session ())
         -> TestName -> FilePath -> UInt -> UInt -> [T.Text] -> TestTree
execCase exec name file l c expected = testCase name $ runExport file $ \doc -> do
    _ <- runAndCheck exec doc (rangeAt l c)
    containsAfter doc expected

addCase, removeCase, exportAllCase :: TestName -> FilePath -> UInt -> UInt -> [T.Text] -> TestTree
addCase       = execCase executeExportAction
removeCase    = execCase executeRemoveAction
exportAllCase = execCase executeExportAllAction

-- | Assert the export or unexport action is not offered at the position.
absentCase :: (TextDocumentIdentifier -> Range -> Session ())
           -> TestName -> FilePath -> UInt -> UInt -> TestTree
absentCase absent name file l c = testCase name $ runExport file $ \doc ->
    absent doc (rangeAt l c)

noCase, noRemoveCase, noExportAllCase :: TestName -> FilePath -> UInt -> UInt -> TestTree
noCase          = absentCase noExportOffered
noRemoveCase    = absentCase noRemoveOffered
noExportAllCase = absentCase noExportAllOffered

-- | Run @act@ at the position, assert the list is well-formed, then run @check@
-- over the resulting document text.
checkCase :: (TextDocumentIdentifier -> Range -> Session T.Text)
          -> TestName -> FilePath -> UInt -> UInt -> (T.Text -> Assertion) -> TestTree
checkCase act name file l c check = testCase name $ runExport file $ \doc -> do
    txt <- act doc (rangeAt l c)
    liftIO (check txt)

exportCase :: TestName -> FilePath -> UInt -> UInt -> (T.Text -> Assertion) -> TestTree
exportCase = checkCase exportAndCheck

findAction :: (T.Text -> Bool) -> TextDocumentIdentifier -> Range -> Session CodeAction
findAction matches doc range = do
    actions <- rights . map toEither <$> getCodeActions doc range
    case find (matches . (^. L.title)) actions of
        Just ca -> pure ca
        Nothing -> liftIO $ assertFailure $
            "no matching code action, saw: " <> show (map (^. L.title) actions)

main :: IO ()
main = defaultTestRunner $ testGroup "Export"
    [ testGroup "Add"
        [ testGroup "declarations"
            [ addCase "add value to export list" "Basic.hs" 7 0  -- on `bar`
                ["Bare, bar) where"]
            , noCase "no action when value already exported" "Basic.hs" 3 0  -- on `foo`
            , addCase "add bare type as T(..)" "Basic.hs" 15 5  -- on `Unlisted` type name
                ["Unlisted(..)", "Unlisted (..)"]
            ]

        , testGroup "constructors"
            [ addCase "constructor with no parent entry appends T (C)" "Basic.hs" 15 16  -- on `Unlisted1`
                ["Unlisted (Unlisted1)", "Unlisted(Unlisted1)"]
            , addCase "constructor under bare-type parent promotes to T(C)" "Basic.hs" 14 12  -- on `Bare1`, Bare is IEThingAbs
                ["Bare (Bare1)", "Bare(Bare1)"]
            , addCase "constructor merges into existing IEThingWith parent" "Basic.hs" 11 25  -- on `Foo3`, Foo has [Foo1, Foo2]
                ["Foo (Foo1, Foo2, Foo3)", "Foo(Foo1, Foo2, Foo3)"]
            , noCase "constructor already in IEThingWith children suppresses action" "Basic.hs" 11 11  -- on `Foo1`
            , noCase "constructor under IEThingAll T(..) suppresses action" "Basic.hs" 12 11  -- on `Qux1`
            ]

        , testGroup "declaration kinds"
            [ addCase "function operator is parenthesized" "AddExportKinds.hs" 8 1  -- on `(<|)`
                ["(placeholder, (<|))"]
            , addCase "type synonym operator has no type keyword" "AddExportKinds.hs" 11 7  -- on `(:<>)`
                ["(placeholder, (:<>))"]
            , addCase "type family operator gets type keyword" "AddExportKinds.hs" 13 14  -- on `(:+:)`
                ["(placeholder, type (:+:))"]
            , addCase "data operator gets type keyword and (..)" "AddExportKinds.hs" 15 7  -- on `(:<)`
                ["placeholder, type (:<)(..)", "placeholder, type (:<) (..)"]
            , addCase "pattern synonym gets a pattern prefix" "AddExportKinds.hs" 18 9  -- on `Pat`
                ["(placeholder, pattern Pat)"]
            , addCase "add class as T(..)" "AddExportKinds.hs" 20 6  -- on `Cls`
                ["placeholder, Cls(..)", "placeholder, Cls (..)"]
            ]

        , testGroup "layout variants"
            [ addCase "add to an empty export list" "AddExportEmpty.hs" 2 0  -- on `foo`
                ["module AddExportEmpty (foo) where"]
            , addCase "append after a trailing comma" "AddExportTrailingComma.hs" 7 0  -- on `bar`
                ["( foo, bar"]
            , exportCase "append to a multi-line leading-comma list keeps the haddock comment" "ExportLayout.hs" 19 0  -- on `quux`
                (`assertContainsAll` ["-- * Types\n  , Foo", "      )\n  , quux\n  ) where"])
            ]

        , testGroup "negative cases"
            [ noCase "no action on implicit module" "Implicit.hs" 3 0
            , noCase "no action when cursor on RHS" "Basic.hs" 7 6  -- on the `2` of `bar = 2`
            , noCase "no action on a where-bound name" "Basic.hs" 19 8  -- on `whereBound`
            , noCase "no action on a record field" "Basic.hs" 21 18  -- on `recField`
            , noCase "no action on a class method" "AddExportKinds.hs" 21 2  -- on `method`
            ]

        , testGroup "CPP in the export list"
            -- EXAMPLE_FLAG is never defined in the test project, so #ifdef branches
            -- are inactive and #ifndef branches are active. The edit must preserve
            -- every directive verbatim and place the new export outside any branch.
            [ testCase "preserves #include directives and a trailing #ifdef block" $
                runExportWith ["CppExportPrelude.h", "CppExportInclude.h"] "CppExport.hs" $ \doc -> do
                    txt <- exportAndCheck doc (rangeAt 21 0)  -- on `extra`
                    liftIO $ do
                        assertContainsAll txt
                            [ "#include \"CppExportPrelude.h\""
                            , "#include \"CppExportInclude.h\""
                            , "( extra, foo"
                            ]
                        assertFlaggedBlockKept "extra" txt

            , testCase "adds a separate entry beside a bare-type parent" $
                runExportWith ["CppExportPrelude.h", "CppExportInclude.h"] "CppExport.hs" $ \doc -> do
                    txt <- exportAndCheck doc (rangeAt 14 11)  -- on `Bar1`, Bar is IEThingAbs
                    liftIO $ do
                        assertFlaggedBlockKept "Bar1" txt
                        txt `assertAnyInfix` ["Bar (Bar1)", "Bar(Bar1)"]

            , exportCase "edit stays valid in the unparsed CPP branch" "CppExportOtherBranch.hs" 12 0 $ \txt -> do  -- on `bar`, the only item is in the other branch
                -- the single item lives under #ifndef, so it is the whole parsed list.
                -- The front-insert plus trailing comma stays valid when the flag flips
                -- and that item disappears.
                let otherBranch = preprocessExampleFlag True txt
                wellFormedExportList otherBranch
                    @? ("edit breaks the EXAMPLE_FLAG-defined configuration:\n" <> T.unpack otherBranch)
            ]
        ]

    , testGroup "Export fixes the unused-binding warning"
        [ knownBrokenForGhcVersions [GHC96]
            "TcRnUnusedName provenance is unstructured before GHC 9.8 (GHC #20115)" $
          testCase "Export action attaches the -Wunused-top-binds diagnostic" $ runExport "Basic.hs" $ \doc -> do
            ca <- findAction (== "Export `bar`") doc (rangeAt 6 0)
            liftIO $ not (null (fromMaybe [] (ca ^. L.diagnostics)))
                @? "Export action should carry the unused-binding diagnostic"
        ]

    , testGroup "Remove"
        [ testGroup "declarations"
            [ removeCase "remove first value (foo)" "Basic.hs" 3 0
                ["module Basic (Bar, Baz (Baz1)"]
            , removeCase "remove bare type (middle item)" "Basic.hs" 9 5  -- on `Bar`
                ["module Basic (foo, Baz (Baz1)"]
            , removeCase "remove IEThingWith type removes whole entry" "Basic.hs" 11 5  -- on `Foo` type
                ["Baz (Baz1), Qux (..)"]
            , removeCase "unexporting the sole export empties the list to ()" "RemoveCtorOp.hs" 4 7  -- on `(:+:)`
                ["module RemoveCtorOp () where"]
            , noRemoveCase "no remove action when value not in export list" "Basic.hs" 7 0  -- `bar` not exported
            ]

        , testGroup "constructors"
            [ removeCase "remove sole constructor downgrades to bare type" "Basic.hs" 10 11  -- on `Baz1` in Baz(Baz1)
                ["(foo, Bar, Baz, Foo"]
            , removeCase "remove last constructor of T(C1, C2) yields T (C1)" "Basic.hs" 11 18  -- on `Foo2` in Foo(Foo1, Foo2)
                ["Foo (Foo1), Qux", "Foo(Foo1), Qux"]
            , removeCase "remove standalone-exported constructor" "Basic.hs" 13 12  -- on `Quux1` standalone
                ["Qux (..), Bare) where"]
            , removeCase "downgrading an operator type keeps the type keyword" "RemoveCtorOp.hs" 4 15  -- on `Op`, the sole constructor of (:+:)
                ["(type (:+:)) where", "(type (:+:) ) where"]
            , noRemoveCase "constructor under IEThingAll suppresses remove action" "Basic.hs" 12 11  -- on `Qux1`, only Qux(..) in list
            , noRemoveCase "constructor not in export list suppresses remove action" "Basic.hs" 11 25  -- on `Foo3`, not in any entry

            -- `data Bar = Bar`: unexporting the constructor leaves the abstract type
            -- exported, so unexport must skip the standalone-removal fallback.
            , noRemoveCase "constructor sharing its type's name does not unexport the type" "Basic.hs" 9 11  -- on the constructor `Bar`
            ]

        , testGroup "layout and comments"
            [ testCase "remove first item keeps own-line layout and drops its comment" $ runExport "ExportLayout.hs" $ \doc -> do
                executeRemoveAction doc (rangeAt 11 0)  -- on `foo`, the first export
                txt <- documentContents doc
                liftIO $ do
                    assertContainsAll txt ["( bar -- the bar\n  -- * Types\n  , Foo\n"]
                    not ("the foo" `T.isInfixOf` txt) @? ("foo's comment survived:\n" <> T.unpack txt)

            , testCase "remove middle item drops its comment, keeps the others'" $ runExport "ExportLayout.hs" $ \doc -> do
                executeRemoveAction doc (rangeAt 14 0)  -- on `bar`, the middle export
                txt <- documentContents doc
                liftIO $ do
                    assertContainsAll txt ["( foo -- the foo\n", "Foo1 -- first"]
                    not ("the bar" `T.isInfixOf` txt) @? ("bar's comment survived:\n" <> T.unpack txt)

            , testCase "removing a middle child keeps the survivors' layout and comments" $ runExport "ExportLayout.hs" $ \doc -> do
                executeRemoveAction doc (rangeAt 17 18)  -- on `Foo2` in the data decl
                txt <- documentContents doc
                liftIO $ do
                    assertContainsAll txt ["( Foo1 -- first\n", "      , Foo3 -- third\n"]
                    not ("second" `T.isInfixOf` txt) @? ("Foo2's comment lingered:\n" <> T.unpack txt)

            , testCase "removing the head child keeps the new head's own comment" $ runExport "ExportLayout.hs" $ \doc -> do
                executeRemoveAction doc (rangeAt 17 11)  -- on `Foo1` in the data decl
                txt <- documentContents doc
                liftIO $ do
                    assertContainsAll txt ["Foo2 -- second", "Foo3 -- third"]
                    not ("first" `T.isInfixOf` txt) @? ("Foo1's comment survived:\n" <> T.unpack txt)
            ]

        , testGroup "negative cases"
            [ noRemoveCase "no remove action when cursor on RHS" "Basic.hs" 4 6  -- on the `1` of `foo = 1`
            -- A reprint would erase the directives the parser stripped, so removal is
            -- declined whenever the export list holds a CPP directive.
            , testCase "no remove action under a CPP export list" $
                runExportWith ["CppExportPrelude.h", "CppExportInclude.h"] "CppExport.hs" $ \doc ->
                    noRemoveOffered doc (rangeAt 12 0)  -- on `foo`, exported beside an #ifdef block
            ]
        ]

    , testGroup "Export all symbols"
        [ exportAllCase "implicit module gets a list of every top-level symbol" "ExportAllImplicit.hs" 1 7
            ["module ExportAllImplicit {-# DEPRECATED \"old\" #-} (value, (<|), Rec (..), Cls (..), Syn, pattern Pat, c_sin, left, right) where"]
        , exportAllCase "explicit list gains the missing symbols of every kind" "AddExportKinds.hs" 3 7
            [ "(placeholder, (<|), (:<>), type (:+:), type (:<) (..), pattern Pat, Cls (..))"
            , "(placeholder, (<|), (:<>), type (:+:), type (:<)(..), pattern Pat, Cls(..))"
            ]
        , testCase "splices the missing symbols in front of a CPP export list" $
            runExportWith ["CppExportPrelude.h", "CppExportInclude.h"] "CppExport.hs" $ \doc -> do
                txt <- runAndCheck executeExportAllAction doc (rangeAt 2 7)
                liftIO $ do
                    assertContainsAll txt
                        [ "#include \"CppExportPrelude.h\""
                        , "#include \"CppExportInclude.h\""
                        ]
                    assertFlaggedBlockKept "extra" txt
        , noExportAllCase "not offered when every symbol is exported" "RemoveCtorOp.hs" 2 7
        , noExportAllCase "not offered off the module header" "Basic.hs" 7 0  -- on `bar`
        ]
    ]
