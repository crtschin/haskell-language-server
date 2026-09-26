-- | Failures of rule and handler code.
module Development.IDE.Core.Internal.Fail
  ( MonadPluginFail (..)
  , required
  , BadDependency (..)
  ) where

import           Control.Exception          (Exception, throwIO)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import qualified Data.Text                  as T
import           Development.IDE.Graph      (Action)
import           Ide.Logger                 (layoutCompact, pretty,
                                             renderStrict)
import           Ide.Plugin.Error           (PluginError (..))

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
