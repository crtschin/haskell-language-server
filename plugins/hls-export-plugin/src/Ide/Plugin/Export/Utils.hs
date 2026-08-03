{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Ide.Plugin.Export.Utils where

import           Control.Concurrent.STM                       (atomically)
import qualified Control.Exception                            as Exception
import           Control.Lens                                 (has)
import           Data.Aeson                                   (FromJSON, ToJSON)
import qualified Data.Map.Strict                              as Map
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Development.IDE                              (IdeAction,
                                                               IdeState)
import           Development.IDE.Core.Shake                   (getDiagnostics)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error             (_TcRnUnusedTopBind,
                                                               msgEnvelopeErrorL)
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Import.DependencyInformation (DependencyInformation (..),
                                                               pathToId)
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Shake                  (WithHieDb)
import           GHC.Generics                                 (Generic)
import qualified HieDb
import           Language.LSP.Protocol.Types
#ifdef hls_cabal
import qualified Ide.Plugin.Cabal.ExposedModules              as Cabal
#endif

-- | Whether the module is public API, and every file the cabal file says gets
-- built. One lookup, because the provider needs both on the same request.
exposureCheck :: NormalizedFilePath -> Text -> IdeAction (Bool, [NormalizedFilePath])
#ifdef hls_cabal
exposureCheck = Cabal.exposureCheck
#else
-- Without cabal support, treat every module as exposed, which withholds the
-- action rather than offering it on what may be a published interface.
exposureCheck _ _ = pure (True, [])
#endif

isModuleExposed :: NormalizedFilePath -> Text -> IdeAction Bool
isModuleExposed nfp modName = fst <$> exposureCheck nfp modName

-- | Which of @declared@ the session has never been told about, meaning a
-- component nobody has loaded.
--
-- See Note [Every consumer must be visible].
unloadedOf :: DependencyInformation -> [NormalizedFilePath] -> [NormalizedFilePath]
unloadedOf depInfo = filter (isNothing . pathToId (depPathIdMap depInfo))

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

-- | Whether hiedb's record for @nfp@ agrees with the @.hie@ file it points at.
-- Says nothing about whether that @.hie@ matches current source. It catches
-- "never indexed" and "index out of step with the interface".
isIndexCurrent :: WithHieDb -> NormalizedFilePath -> IO Bool
isIndexCurrent withDb nfp = do
  mrow <- withDb $ \db -> HieDb.lookupHieFileFromSource db (fromNormalizedFilePath nfp)
  case mrow of
    Nothing -> pure False
    Just row -> do
      hashed <- Exception.try @Exception.IOException
        (getFileHash (HieDb.hieModuleHieFile row))
      pure $ either (const False) (== HieDb.modInfoHash (HieDb.hieModInfo row)) hashed

-- | The resolve payload for "Export explicitly". A single mode for now, kept as
-- a type so the data field round-trips through resolve.
data ExportResolveData = ExportUsed
  deriving Generic

instance ToJSON ExportResolveData
instance FromJSON ExportResolveData
