{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Ide.Plugin.Export.Utils where

import           Control.Concurrent.STM            (atomically, check, readTVar)
import           Control.Lens                      (has)
import           Data.Aeson                        (FromJSON, ToJSON)
import qualified Data.HashMap.Strict               as HMap
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (isNothing)
import           Data.Text                         (Text)
import           Development.IDE                   (IdeAction, IdeState,
                                                    ShakeExtras)
import           Development.IDE.Core.Shake        (HieDbWriter (..),
                                                    getDiagnostics, hiedbWriter)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error  (_TcRnUnusedTopBind,
                                                    msgEnvelopeErrorL)
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Types.Diagnostics
import           GHC.Generics                      (Generic)
import           GHC.Types.TypeEnv                 (TypeEnv, lookupTypeEnv)
import           Language.LSP.Protocol.Types
#ifdef hls_cabal
import qualified Ide.Plugin.Cabal.ExposedModules   as Cabal
#endif

-- | Whether the module is in a library's @exposed-modules@, making its exports
-- public API.
isModuleExposed :: NormalizedFilePath -> Text -> IdeAction Bool
#ifdef hls_cabal
isModuleExposed = Cabal.isModuleExposed
#else
-- Without cabal support compiled in, treat every module as exposed.
isModuleExposed _ _ = pure True
#endif

rdrNameFS :: RdrName -> FastString
rdrNameFS = occNameFS . rdrNameOcc

ieParentName :: IE GhcPs -> Maybe RdrName
ieParentName e = case e of
#if MIN_VERSION_ghc(9,9,0)
  IEVar _ (L _ wn) _           -> Just (ieWrappedRdrName wn)
  IEThingAbs _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingAll _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingWith _ (L _ wn) _ _ _ -> Just (ieWrappedRdrName wn)
#else
  IEVar _ (L _ wn)             -> Just (ieWrappedRdrName wn)
  IEThingAbs _ (L _ wn)        -> Just (ieWrappedRdrName wn)
  IEThingAll _ (L _ wn)        -> Just (ieWrappedRdrName wn)
  IEThingWith _ (L _ wn) _ _   -> Just (ieWrappedRdrName wn)
#endif
  _                            -> Nothing

-- | The listed constructors of an @IEThingWith@ (@T(C1, C2)@), or 'Nothing' otherwise.
ieThingWithChildren :: IE GhcPs -> Maybe [LIEWrappedName GhcPs]
#if MIN_VERSION_ghc(9,9,0)
ieThingWithChildren (IEThingWith _ _ _ cs _) = Just cs
#else
ieThingWithChildren (IEThingWith _ _ _ cs)   = Just cs
#endif
ieThingWithChildren _                        = Nothing

-- | The head name of an @IEThingWith@, e.g. @T@ in @T(C1, C2)@.
ieThingWithHead :: IE GhcPs -> Maybe (LIEWrappedName GhcPs)
#if MIN_VERSION_ghc(9,9,0)
ieThingWithHead (IEThingWith _ n _ _ _) = Just n
#else
ieThingWithHead (IEThingWith _ n _ _)   = Just n
#endif
ieThingWithHead _                       = Nothing

ieWrappedRdrName :: IEWrappedName GhcPs -> RdrName
ieWrappedRdrName = \case
  IEName _ (L _ rdr)    -> rdr
  IEPattern _ (L _ rdr) -> rdr
  IEType _ (L _ rdr)    -> rdr
#if MIN_VERSION_ghc(9,11,0)
  IEDefault _ (L _ rdr) -> rdr
#endif
#if MIN_VERSION_ghc(9,13,0)
  IEData _ (L _ rdr)    -> rdr
#endif

-- | True when the export item's head name is the given 'FastString'.
parentNameIs :: FastString -> IE GhcPs -> Bool
parentNameIs fs = maybe False ((== fs) . rdrNameFS) . ieParentName

-- | The 'FastString' of a located wrapped name, e.g. an @IEThingWith@ child.
lieWrappedNameFS :: LIEWrappedName GhcPs -> FastString
lieWrappedNameFS = rdrNameFS . ieWrappedRdrName . unLoc

-- | True when @n@ is listed as a child constructor of an @IEThingWith@.
isInIE :: FastString -> IE GhcPs -> Bool
isInIE n = maybe False (any ((== n) . lieWrappedNameFS)) . ieThingWithChildren

singleFileEdit :: Uri -> [TextEdit] -> WorkspaceEdit
singleFileEdit uri edits = WorkspaceEdit (Just (Map.singleton uri edits)) Nothing Nothing

mkAction :: Text -> CodeAction
mkAction title = CodeAction {..}
  where
    _title = title
    _kind = Just CodeActionKind_RefactorRewrite
    _diagnostics = Nothing
    _isPreferred = Nothing
    _disabled = Nothing
    _edit = Nothing
    _command = Nothing
    _data_ = Nothing

-- | The LSP diagnostics for names GHC reports as unused top-level definitions.
unusedTopBindDiagnostics :: IdeState -> NormalizedFilePath -> IO [Diagnostic]
unusedTopBindDiagnostics state nfp = do
  diags <- atomically $ getDiagnostics state
  pure [fdLspDiagnostic d | d <- diags, fdFilePath d == nfp, isUnusedTopBind d]
  where
    isUnusedTopBind =
      has (fdStructuredMessageL . _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnUnusedTopBind)

-- | A data type's exportable children.
parentChildren :: TypeEnv -> Name -> Maybe [Name]
parentChildren typeEnv parent = case lookupTypeEnv typeEnv parent of
  Just (ATyCon tc)
    | isNothing (tyConClass_maybe tc) ->
        Just (map getName (tyConDataCons tc) ++ map flSelector (tyConFieldLabels tc))
  _ -> Nothing

-- | Block until hiedb has finished indexing every file in @fps@.
awaitIndexed :: ShakeExtras -> [NormalizedFilePath] -> IO ()
awaitIndexed extras fps = atomically $ do
  pending <- readTVar (indexPending (hiedbWriter extras))
  check (not (any (`HMap.member` pending) fps))

-- | The resolve payload for "Export explicitly". A single mode for now, kept as
-- a type so the data field round-trips through resolve.
data ExportResolveData = ExportUsed
  deriving Generic

instance ToJSON ExportResolveData
instance FromJSON ExportResolveData
