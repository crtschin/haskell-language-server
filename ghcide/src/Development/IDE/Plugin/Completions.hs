{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}

module Development.IDE.Plugin.Completions
    ( descriptor
    , Log(..)
    , ghcideCompletionsPluginPriority
    ) where

import           Control.Concurrent.Async                 (concurrently)
import           Control.Concurrent.STM.Stats             (readTVarIO)
import           Control.Lens                             ((&), (.~), (?~))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class                (lift)
import qualified Data.HashMap.Strict                      as Map
import qualified Data.HashSet                             as Set
import           Data.Maybe
import qualified Data.Text                                as T
import           Development.IDE.Core.API                 (Tracked (..), await,
                                                           fetch, fetch_,
                                                           noFile, ok, output,
                                                           recall, rule,
                                                           runQuery, settle,
                                                           untrack,
                                                           withRuleRecorder)
import           Development.IDE.Core.Compile
import           Development.IDE.Core.Internal.Tracked    (unsafeUnAge)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service             hiding (Log, LogShake)
import           Development.IDE.Core.Shake               hiding (Log,
                                                           knownTargets, use)
import qualified Development.IDE.Core.Shake               as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util
import           Development.IDE.Graph
import           Development.IDE.Plugin.Completions.Logic
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Spans.Common
import           Development.IDE.Spans.Documentation
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq           (HscEnvEq (envPackageExports),
                                                           hscEnv)
import qualified Development.IDE.Types.KnownTargets       as KT
import           Development.IDE.Types.Location
import           Ide.Logger                               (Pretty (pretty),
                                                           Recorder,
                                                           WithPriority,
                                                           cmapWithPrio)
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Numeric.Natural
import           Prelude                                  hiding (mod)
import           Text.Fuzzy.Parallel                      (Scored (..))

import           Development.IDE.Core.Rules               (usePropertyAction)

import qualified Ide.Plugin.Config                        as Config

import           Development.IDE.Types.Options            (LinkTargets (..),
                                                           linkTargets)
import qualified GHC.LanguageExtensions                   as LangExt

data Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake msg -> pretty msg

ghcideCompletionsPluginPriority :: Natural
ghcideCompletionsPluginPriority = defaultPluginPriority

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId desc)
  { pluginRules = produceCompletions recorder
  , pluginHandlers = mkPluginHandler SMethod_TextDocumentCompletion getCompletionsLSP
                     <> mkResolveHandler SMethod_CompletionItemResolve resolveCompletion
  , pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
  , pluginPriority = ghcideCompletionsPluginPriority
  }
  where
    desc = "Provides Haskell completions"


produceCompletions :: Recorder (WithPriority Log) -> Rules ()
produceCompletions recorder = do
  withRuleRecorder (cmapWithPrio LogShake recorder) $ do
    rule $ \LocalCompletions file -> do
        let uri = fromNormalizedUri $ normalizedFilePathToUri file
        mbPm <- recall GetParsedModule file
        -- The completions keep the spans of a parse that can be stale.
        pure $ output $ (\(Tracked pm _) -> localCompletionsForParsedModule uri (unsafeUnAge pm)) <$> mbPm
    rule $ \NonLocalCompletions file -> do
        -- For non local completions we avoid depending on the parsed module,
        -- synthesizing a fake module with an empty body from the buffer
        -- in the ModSummary, which preserves all the imports
        -- The completions keep the spans of imports that can be stale.
        ms <- fmap (\(Tracked m _) -> unsafeUnAge m) <$> recall GetModSummaryWithoutTimestamps file
        mbSess <- fmap untrack <$> recall GhcSessionDeps file

        case (ms, mbSess) of
            (Just ModSummaryResult{..}, Just sess) -> do
              let env = hscEnv sess
              -- We do this to be able to provide completions of items that are not restricted to the explicit list
              (global, inScope) <- liftIO $ tcRnImportDecls env (dropListFromImportDecl <$> msrImports) `concurrently` tcRnImportDecls env msrImports
              case (global, inScope) of
                  ((_, Just globalEnv), (_, Just inScopeEnv)) -> do
                      let visibleMods = listVisibleModuleNames $ hscEnv sess
                      let uri = fromNormalizedUri $ normalizedFilePathToUri file
                      let cdata = cacheDataProducer uri visibleMods (ms_mod msrModSummary) globalEnv inScopeEnv msrImports
                      pure (ok cdata)
                  (_diag, _) ->
                      pure (output Nothing)
            _ -> pure (output Nothing)

-- Drop any explicit imports in ImportDecl if not hidden
dropListFromImportDecl :: LImportDecl GhcPs -> LImportDecl GhcPs
dropListFromImportDecl iDecl = let
    f d@ImportDecl {ideclImportList} = case ideclImportList of
        Just (Exactly, _) -> d {ideclImportList=Nothing}
        -- if hiding or Nothing just return d
        _                 -> d
    f x = x
    in f <$> iDecl

resolveCompletion :: ResolveFunction IdeState CompletionResolveData Method_CompletionItemResolve
resolveCompletion ide _pid comp@CompletionItem{_detail,_documentation,_data_} uri (CompletionResolveData _ needType (NameDetails mod occ)) =
  do
    file <- getNormalizedFilePathE uri
    sess <- handleMaybe PluginStaleResolve . fmap untrack
              =<< runQuery ide (settle GhcSessionDeps file)
    let nc = ideNc $ shakeExtras ide
    name <- liftIO $ lookupNameCache nc mod occ
    mdkm <- runQuery ide $ settle GetDocMap file
    let (dm,km) = case untrack <$> mdkm of
          Just (DKMap docMap tyThingMap _argDocMap) -> (docMap,tyThingMap)
          Nothing                                   -> (mempty, mempty)
    doc <- case lookupNameEnv dm name of
      Just doc -> pure $ spanDocToMarkdown doc
      Nothing -> liftIO $ do
        ltgts <- linkTargets <$> getIdeOptionsIO (shakeExtras ide)
        spanDocToMarkdown . fst <$> getDocumentationTryGhc (hscEnv sess) ltgts name
    typ <- case lookupNameEnv km name of
      _ | not needType -> pure Nothing
      Just ty -> pure (safeTyThingType True ty)
      Nothing -> do
        (safeTyThingType True =<<) <$> liftIO (lookupName (hscEnv sess) name)
    let det1 = case typ of
          Just ty -> Just (":: " <> printOutputable (stripForall ty) <> "\n")
          Nothing -> Nothing
        doc1 = case _documentation of
          Just (InR (MarkupContent MarkupKind_Markdown old)) ->
            InR $ MarkupContent MarkupKind_Markdown $ T.intercalate sectionSeparator (old:doc)
          _ -> InR $ MarkupContent MarkupKind_Markdown $ T.intercalate sectionSeparator doc
    pure  (comp & L.detail .~ (det1 <> _detail)
                & L.documentation ?~ doc1)
  where
    stripForall ty = case splitForAllTyCoVars ty of
      (_,res) -> res

-- | Generate code actions.
getCompletionsLSP :: PluginMethodHandler IdeState Method_TextDocumentCompletion
getCompletionsLSP ide plId
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contentsMaybe <- runQuery ide $
      maybe (pure Nothing) (fmap snd . fetch_ GetFileContents) (uriToNormalizedFilePath $ toNormalizedUri uri)
    case (contentsMaybe, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        (ideOpts, compls, moduleExports, astres) <- runQuery ide $ do
            opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            localCompls <- settle LocalCompletions npath
            nonLocalCompls <- settle NonLocalCompletions npath
            pm <- settle GetParsedModule npath
            binds <- settle GetBindings npath
            knownTargets <- fetch GetKnownTargets noFile
            let localModules = maybe [] (Map.keys . targetMap) knownTargets
            let lModules = mempty{importableModules = map toModueNameText localModules}
            -- set up the exports map including both package and project-level identifiers
            packageExportsMapIO <- fmap (envPackageExports . untrack) <$> settle GhcSession npath
            packageExportsMap <- mapM liftIO packageExportsMapIO
            projectExportsMap <- liftIO $ readTVarIO (exportsMap $ shakeExtras ide)
            let exportsMap = fromMaybe mempty packageExportsMap <> projectExportsMap

            let moduleExports = getModuleExportsMap exportsMap
                exportsCompItems = foldMap (map (fromIdentInfo uri) . Set.toList) . nonDetOccEnvElts . getExportsMap $ exportsMap
                exportsCompls = mempty{anyQualCompls = exportsCompItems}
            -- The cached completions keep spans that can be stale.
            let cached (Tracked c _) = unsafeUnAge c
                compls = (cached <$> localCompls) <> (cached <$> nonLocalCompls) <> Just exportsCompls <> Just lModules

            -- get HieAst if OverloadedRecordDot is enabled
            dflags <- fmap (untrack . fmap (ms_hspp_opts . msrModSummary)) <$> settle GetModSummaryWithoutTimestamps npath
            astres <- case dflags of
              Just dflags' | xopt LangExt.OverloadedRecordDot dflags'
                ->  settle GetHieAst npath
              _ -> return Nothing

            pure (opts, fmap (,pm,binds) compls, moduleExports, astres)
        case compls of
          Just (cci', parsedMod, bindMap) -> do
            let pfix = getCompletionPrefixFromRope position cnts
            case (pfix, completionContext) of
              (PosPrefixInfo _ "" _ _, Just CompletionContext { _triggerCharacter = Just "."})
                -> return (InL [])
              (_, _) -> do
                let clientCaps = clientCapabilities $ shakeExtras ide
                    plugins = idePlugins $ shakeExtras ide
                config <- runQuery ide $ await "Completion.config" (lift $ getCompletionsConfig plId)

                let allCompletions = getCompletions plugins ideOpts cci' parsedMod astres bindMap pfix clientCaps config moduleExports uri
                pure $ InL (orderedCompletions allCompletions)
          _ -> return (InL [])
      _ -> return (InL [])

getCompletionsConfig :: PluginId -> Action CompletionsConfig
getCompletionsConfig pId =
  CompletionsConfig
    <$> usePropertyAction #snippetsOn pId properties
    <*> usePropertyAction #autoExtendOn pId properties
    <*> (Config.maxCompletions <$> getClientConfigAction)

{- COMPLETION SORTING
   We return an ordered set of completions (local -> nonlocal -> global).
   Ordering is important because local/nonlocal are import aware, whereas
   global are not and will always insert import statements, potentially redundant.

   Moreover, the order prioritizes qualifiers, for instance, given:

   import qualified MyModule
   foo = MyModule.<complete>

   The identifiers defined in MyModule will be listed first, followed by other
   identifiers in importable modules.

   According to the LSP specification, if no sortText is provided, the label is used
   to sort alphabetically. Alphabetical ordering is almost never what we want,
   so we force the LSP client to respect our ordering by using a numbered sequence.
-}

orderedCompletions :: [Scored CompletionItem] -> [CompletionItem]
orderedCompletions [] = []
orderedCompletions xx = zipWith addOrder [0..] xx
    where
    lxx = digits $ Prelude.length xx
    digits = Prelude.length . show

    addOrder :: Int -> Scored CompletionItem -> CompletionItem
    addOrder n Scored{original = it@CompletionItem{_label,_sortText}} =
        it{_sortText = Just $
                T.pack(pad lxx n)
                }

    pad n x = let sx = show x in replicate (n - Prelude.length sx) '0' <> sx

----------------------------------------------------------------------------------------------------

toModueNameText :: KT.Target -> T.Text
toModueNameText target = case target of
  KT.TargetModule m -> T.pack $ moduleNameString m
  _                 -> T.empty
