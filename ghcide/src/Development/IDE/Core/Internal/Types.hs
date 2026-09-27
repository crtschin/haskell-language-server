{-# LANGUAGE TypeFamilies #-}
-- | Keys of rules with their scope, and failures of rule and handler code.
module Development.IDE.Core.Internal.Types
  ( -- * Keys
    IdeRule
  , Key (..)
  , Project (..)
  , IsScope (..)
    -- * Failures
  , MonadPluginFail (..)
  , required
  , BadDependency (..)
  ) where

import           Control.DeepSeq                (NFData)
import           Control.Exception              (Exception, throwIO)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Except     (ExceptT, throwE)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import qualified Data.Text                      as T
import           Data.Typeable                  (Typeable)
import           Development.IDE.Graph          (Action, RuleResult, ShakeValue)
import           Development.IDE.Types.Location (NormalizedFilePath,
                                                 emptyFilePath)
import           Ide.Logger                     (layoutCompact, pretty,
                                                 renderStrict)
import           Ide.Plugin.Error               (PluginError (..))

type IdeRule k v =
  ( RuleResult k ~ v
  , ShakeValue k
  , Show v
  , Typeable v
  , NFData v
  )

-- | The scope of a key that has one value for the whole project.
data Project = Project

class IsScope s where
  scopeFile :: s -> NormalizedFilePath

instance IsScope NormalizedFilePath where
  scopeFile = id

instance IsScope Project where
  scopeFile _ = emptyFilePath

-- | A key that can be read, for each file or for the whole 'Project':
--
-- @
-- instance Key GetKnownTargets where
--   type Scope GetKnownTargets = Project
-- @
class (IdeRule k (RuleResult k), IsScope (Scope k)) => Key k where
  type Scope k
  type Scope k = NormalizedFilePath

-- | When we depend on something that reported an error, and we fail as a direct result, throw BadDependency
--   which short-circuits the rest of the action
newtype BadDependency = BadDependency String deriving Show
instance Exception BadDependency

-- | Monads that can fail with a 'PluginError'.
class Monad m => MonadPluginFail m where
  pluginFailed :: PluginError -> m a

-- | The build catches 'BadDependency'. We do not convert these into
-- diagnostics, as the dependency would have already done that.
instance MonadPluginFail Action where
  pluginFailed = \case
    PluginRuleFailed t -> liftIO $ throwIO $ BadDependency (T.unpack t)
    e -> fail $ T.unpack $ renderStrict $ layoutCompact $ pretty e
instance Monad m => MonadPluginFail (ExceptT PluginError m) where
  pluginFailed = throwE
instance Monad m => MonadPluginFail (MaybeT m) where
  pluginFailed _ = MaybeT (pure Nothing)
instance MonadPluginFail Maybe where
  pluginFailed _ = Nothing

-- | Fails with the error if there is no value.
required :: MonadPluginFail m => PluginError -> Maybe a -> m a
required e = maybe (pluginFailed e) pure
