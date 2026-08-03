{-# LANGUAGE CPP #-}
{- HLINT ignore "Avoid restricted function" -}

module Ide.Plugin.Export.Utils
  ( exposureCheck
  , exposureCheckFast
  , rdrNameFS
  , ieParentName
  , ThingWith (..)
  , ieThingWithParts
  , parentNameIs
  , lieWrappedNameFS
  , singleFileEdit
  , mkAction
  , unusedTopBindDiagnostics
  , ExportResolveData (..)
  , isCpp
  , isWholeProjectLoading
  , lexicalOrder
  , modNameText
  ) where

import           Control.Concurrent.STM            (atomically)
import           Control.Lens                      (has)
import           Control.Monad.Except              (ExceptT)
import           Control.Monad.Trans.Class         (lift)
import           Data.Aeson                        (FromJSON, ToJSON)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (listToMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Development.IDE                   (Action)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error  (_TcRnUnusedTopBind,
                                                    msgEnvelopeErrorL)
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Types.Diagnostics
import           GHC.Generics                      (Generic)
import qualified GHC.LanguageExtensions.Type       as LangExt
import qualified Ide.Plugin.Cabal.ExposedModules   as Cabal
import           Ide.Plugin.Config
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Types

-- | Whether the module is public API.
exposureCheck :: NormalizedFilePath -> Text -> Action Bool
exposureCheck = Cabal.exposureCheckWith use

-- | Whether the module is public API.
exposureCheckFast :: NormalizedFilePath -> Text -> IdeAction Bool
exposureCheckFast = Cabal.exposureCheckWith $ \k nfp -> fmap fst <$> useWithStaleFast k nfp

rdrNameFS :: RdrName -> FastString
rdrNameFS = occNameFS . rdrNameOcc

-- | The head name of an export item, 'Nothing' for a headless one such as a
-- @module M@ re-export or a doc chunk.
ieParentName :: IE GhcPs -> Maybe RdrName
ieParentName = listToMaybe . ieNames

-- | An @IEThingWith@ (@T(C1, C2)@) taken apart.
data ThingWith = ThingWith
  { head     :: LIEWrappedName GhcPs
    -- ^ @T@. Keeps its wrapping, so @type (:<)(C)@ downgrades to @type (:<)@.
  , children :: [LIEWrappedName GhcPs]
    -- ^ The listed constructors, fields, or methods.
  , rebuild  :: [LIEWrappedName GhcPs] -> IE GhcPs
    -- ^ Put new children back and keep every other field of the original.
  }

-- | Take an @IEThingWith@ apart.
ieThingWithParts :: IE GhcPs -> Maybe ThingWith
#if MIN_VERSION_ghc(9,9,0)
ieThingWithParts (IEThingWith x n w cs docs) =
  Just (ThingWith n cs (\cs' -> IEThingWith x n w cs' docs))
#else
ieThingWithParts (IEThingWith x n w cs) =
  Just (ThingWith n cs (\cs' -> IEThingWith x n w cs'))
#endif
ieThingWithParts _ = Nothing

-- | True when the export item's head name is the given 'FastString'.
parentNameIs :: FastString -> IE GhcPs -> Bool
parentNameIs fs = maybe False ((== fs) . rdrNameFS) . ieParentName

-- | The 'FastString' of a located wrapped name, e.g. an @IEThingWith@ child.
lieWrappedNameFS :: LIEWrappedName GhcPs -> FastString
lieWrappedNameFS = rdrNameFS . lieWrappedName

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

unusedTopBindDiagnostics :: IdeState -> NormalizedFilePath -> IO [Diagnostic]
unusedTopBindDiagnostics state nfp = do
  diags <- atomically $ getDiagnostics state
  pure [fdLspDiagnostic d | d <- diags, fdFilePath d == nfp, isUnusedTopBind d]
  where
    isUnusedTopBind =
      has (fdStructuredMessageL . _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnUnusedTopBind)

-- | Empty resolve payload for "Export explicitly".
data ExportResolveData = ExportUsed
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

isWholeProjectLoading :: IdeState -> ExceptT PluginError (HandlerM Config) Bool
isWholeProjectLoading state = do
  config <- runActionE "Export.clientConfig" state (lift getClientConfigAction)
  pure (componentsLoading config == PreferMultiWholeProjectLoading)

isCpp :: ModSummary -> Bool
isCpp = xopt LangExt.Cpp . ms_hspp_opts

lexicalOrder :: GenLocated l (IE GhcPs) -> Maybe LexicalFastString
lexicalOrder = fmap (LexicalFastString . rdrNameFS) . ieParentName . unLoc

modNameText :: ModSummary -> Text
modNameText = T.pack . moduleNameString . moduleName . ms_mod
