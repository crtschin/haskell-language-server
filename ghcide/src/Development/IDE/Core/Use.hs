-- | The query API for rules and handlers.
--
-- @
--                     current     last known
--   rule, waits       use         recall
--   handler, waits    fetch       refresh
--   handler, no wait              snapshot
-- @
--
-- In general 'snapshot' is only useful when there are persisted results. Prefer
-- 'settle', which only waits if the rule hasn't ran before.
--
-- Dependencies are recorded for keys used in 'Action', intended for rules.
-- Dependencies are not recorded in 'Query', intended for handlers.
--
-- A current value comes from a build of the current version of the file. A
-- "last known" value is the last value that succeeded.
--
-- == Examples
--
-- With 'use_', a rule reads the current value of another key and reruns when
-- that value changes:
--
-- @
-- tmr <- 'use_' TypeCheck file
-- @
--
-- With 'uses_', a rule builds one key for many files in one batch:
--
-- @
-- ifaces <- 'uses_' GetModIface deps
-- @
--
-- With 'recall_', a rule reuses the last value that succeeded, e.g. while the
-- file does not parse:
--
-- @
-- 'Tracked' parsed mapping <- 'recall_' GetParsedModule file
-- @
--
-- With 'settle_', a handler returns the last known value and waits only on the
-- first call:
--
-- @
-- 'Tracked' ast mapping <- 'settle_' GetHieAst file
-- oldPos <- 'rewind' mapping pos
-- @
--
-- With 'refresh_', a handler waits for a new build and reads the last known
-- value:
--
-- @
-- 'Tracked' sigs mapping <- 'runQuery' ideState $ 'refresh_' GetGlobalBindingTypeSigs file
-- @
--
-- With 'fastForward', a value of a last known version moves to the current
-- version:
--
-- @
-- highlights <- 'fastForward' mapping (documentHighlights ast oldPos)
-- @
--
-- With 'untrack', a handler reads a last known value of an 'Ageless' type:
--
-- @
-- env <- 'untrack' \<$\> 'settle_' GhcSession file
-- dflags <- 'untrack' . fmap (ms_hspp_opts . msrModSummary) \<$\> 'settle_' GetModSummary file
-- @
--
-- With 'fetch_', a handler waits for the current value:
--
-- @
-- tmr <- 'runQuery' ideState $ 'fetch_' TypeCheck file
-- @
--
-- With 'request', a handler can specify when to wait for the current value:
--
-- @
-- wait <- 'request' TypeCheck (Identity file)
-- tmr <- liftIO wait
-- @
--
-- With 'noFile', a rule reads from a global singleton key:
--
-- @
-- settings <- 'use_' GetClientSettings 'noFile'
-- @
module Development.IDE.Core.Use
  ( -- * Primitives
    uses
  , recalls
  , request
  , snapshot
  , await
  , runQuery
  , Query
    -- * Transformers
  , must
  , MonadAction
  , MonadQuery
  , MonadPluginFail (..)
  , required
    -- * Derived
  , settles
  , use
  , use_
  , uses_
  , recall
  , recall_
  , settle
  , settle_
  , refreshes
  , refresh
  , refresh_
  , fetches
  , fetch
  , fetch_
  , noFile
    -- * Positions of last known values
  , Aged
  , fromVersionOf
  , PositionMap
  , Tracked (..)
  , Remap
  , inFile
  , Remappable (..)
  , rewind
  , fastForward
  , fastForwardEach
  , rewindBounds
  , Ageless
  , ageless
  , untrack
  ) where

import           Control.Exception                     (try)
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Maybe
import qualified Data.Text                             as T
import           Development.IDE.Core.Internal
import           Development.IDE.Core.Internal.Fail
import           Development.IDE.Core.Internal.Tracked
import           Development.IDE.Graph
import           Development.IDE.Graph.Rule
import           Development.IDE.Types.Location
import           Development.IDE.Types.Shake
import           Ide.Logger
import           Ide.Plugin.Error
import Control.Monad.Extra (eitherM)

-- | Reads the current value of the key for each file.
--
-- Use this in rule definitions. The rule records a dependency on each value,
-- and reruns when a value changes.
uses
  :: (Traversable t, IdeRule k v, MonadAction m)
  => k -> t NormalizedFilePath -> m (t (Maybe v))
uses k fs = liftAction $ fmap (\(A value) -> currentValue value) <$> apply (fmap (Q . (k,)) fs)

-- | Reads the last known value of the key for each file.
--
-- Use this in rules that can work with older values, e.g. while the file does
-- not parse. The rule records a dependency on each value.
recalls
  :: (Traversable t, IdeRule k v, MonadAction m)
  => k -> t NormalizedFilePath -> m (t (Maybe (Tracked v)))
recalls k fs = liftAction $ do
  _ <- apply (fmap (Q . (k,)) fs)
  extras <- getShakeExtras
  liftIO $ lastValues extras k fs

-- | Queues a build of the key for each file. The result is an action that
-- waits for the up-to-date value.
--
-- Use this in handlers where you want to fire off async behavior.
request
  :: (Traversable t, IdeRule k v, MonadQuery m)
  => k -> t NormalizedFilePath -> m (IO (t (Maybe v)))
request k fs = liftQuery $ enqueue herald (uses k fs)
  where
    herald = "C:" ++ show k ++ ":" ++ unwords (map fromNormalizedFilePath (toList fs))

-- | Reads the last known value of the key for each file. It does not queue a
-- build. If the store has no value, it reads the value from the disk.
--
-- Use it handlers that have to be fast where outdated values are permitted and
-- where we use persistent artifacts.
snapshot
  :: (Traversable t, IdeRule k v, MonadQuery m)
  => k -> t NormalizedFilePath -> m (t (Maybe (Tracked v)))
snapshot k fs = liftQuery $ do
  extras <- ask
  liftIO $ lastValues extras k fs

lastValues
  :: (Traversable t, IdeRule k v)
  => ShakeExtras -> k -> t NormalizedFilePath -> IO (t (Maybe (Tracked v)))
lastValues extras k = traverse (\f -> fmap (tracked f) <$> lastValueIO extras k f)

-- | Runs a query in a handler.
runQuery :: (MonadIO m, MonadPluginFail m) => IdeState -> Query a -> m a
runQuery ide q =
  eitherM (\(QueryFailed e) -> pluginFailed e) pure
    $ liftIO (try (runReaderT (runQueryT q) (shakeExtras ide)))

enqueue :: String -> Action a -> Query (IO a)
enqueue herald = delayedAction . mkDelayedAction herald Debug

one
  :: Functor m
  => (forall t. Traversable t => k -> t NormalizedFilePath -> m (t r))
  -> k -> NormalizedFilePath -> m r
one q k f = runIdentity <$> q k (Identity f)

type MonadAction m = MonadBase Action m

type MonadQuery m = MonadBase Query m

liftAction :: MonadAction m => Action a -> m a
liftAction = liftBase

liftQuery :: MonadQuery m => Query a -> m a
liftQuery = liftBase

must
  :: (Traversable t, Show k, MonadPluginFail m)
  => (k -> t NormalizedFilePath -> m (t (Maybe a)))
  -> k -> t NormalizedFilePath -> m (t a)
must q k fs = q k fs >>= required (PluginRuleFailed (T.pack (show k))) . sequenceA

-- | Queues a build of the key for each file, and reads the last known values.
-- Only blocks if there is no last known value.
--
-- Use it in a handler that have to be fast and run often. Where an outdated
-- value is likely to be updated, e.g. completions or hovers.
settles
  :: (Traversable t, IdeRule k v, MonadQuery m)
  => k -> t NormalizedFilePath -> m (t (Maybe (Tracked v)))
settles k fs = liftQuery $ do
  done <- request k fs
  ran <- traverse (hasRun k) fs
  s <- snapshot k fs
  if and (zipWith (||) (toList ran) (isJust <$> toList s))
    then pure s
    else liftIO done *> snapshot k fs

-- | Queues a build of the key for each file and waits for it. If the build
-- fails, the result is the value of an earlier build.
--
-- Use it in handlers that ideally use up-to-date values, but we permit using
-- with outdated values as well, e.g. code-actions or resolves.
refreshes
  :: (Traversable t, IdeRule k v, MonadQuery m)
  => k -> t NormalizedFilePath -> m (t (Maybe (Tracked v)))
refreshes k fs = liftQuery $ do
  done <- request k fs
  _ <- liftIO done
  snapshot k fs

-- | Queues a build of the key for each file, waits for it, and reads the
-- current values.
--
-- Use it handlers that need current version of values, e.g. to edit the file.
fetches
  :: (Traversable t, IdeRule k v, MonadQuery m)
  => k -> t NormalizedFilePath -> m (t (Maybe v))
fetches k fs = liftQuery $ liftIO =<< request k fs

use :: (IdeRule k v, MonadAction m) => k -> NormalizedFilePath -> m (Maybe v)
use = one uses

use_ :: (IdeRule k v, MonadAction m, MonadPluginFail m) => k -> NormalizedFilePath -> m v
use_ = one (must uses)

uses_
  :: (Traversable t, IdeRule k v, MonadAction m, MonadPluginFail m)
  => k -> t NormalizedFilePath -> m (t v)
uses_ = must uses

recall :: (IdeRule k v, MonadAction m) => k -> NormalizedFilePath -> m (Maybe (Tracked v))
recall = one recalls

recall_
  :: (IdeRule k v, MonadAction m, MonadPluginFail m)
  => k -> NormalizedFilePath -> m (Tracked v)
recall_ = one (must recalls)

settle :: (IdeRule k v, MonadQuery m) => k -> NormalizedFilePath -> m (Maybe (Tracked v))
settle = one settles

settle_
  :: (IdeRule k v, MonadQuery m, MonadPluginFail m)
  => k -> NormalizedFilePath -> m (Tracked v)
settle_ = one (must settles)

refresh :: (IdeRule k v, MonadQuery m) => k -> NormalizedFilePath -> m (Maybe (Tracked v))
refresh = one refreshes

refresh_
  :: (IdeRule k v, MonadQuery m, MonadPluginFail m)
  => k -> NormalizedFilePath -> m (Tracked v)
refresh_ = one (must refreshes)

fetch :: (IdeRule k v, MonadQuery m) => k -> NormalizedFilePath -> m (Maybe v)
fetch = one fetches

fetch_
  :: (IdeRule k v, MonadQuery m, MonadPluginFail m)
  => k -> NormalizedFilePath -> m v
fetch_ = one (must fetches)

-- | Runs a rule action from a handler, and waits for it.
--
-- Use it when a handler reads multiple rules in a single action. Can rerun when
-- interrupted by restarts.
await
  :: (MonadQuery m, MonadPluginFail m)
  => String -> ExceptT PluginError Action a -> m a
await herald act = do
  r <- liftQuery $ liftIO . try =<< enqueue herald (runExceptT act)
  -- A read in a plain 'Action' fails with 'BadDependency', not 'PluginError'.
  either (\(BadDependency dep) -> pluginFailed (PluginRuleFailed (T.pack dep)))
    (either pluginFailed pure) r

-- | The file path for a key that has one value for the whole project.
noFile :: NormalizedFilePath
noFile = emptyFilePath
