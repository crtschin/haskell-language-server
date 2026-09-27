-- | Reads of rule results, for rules and handlers.
--
-- @
--            current     last known
--   use      rule        rule
--   await    handler     handler, after a build
--   peek                 handler, waits only for cold keys
-- @
--
-- == Examples
--
-- '<*>' and 'traverse' batch reads, and '>>=' allows dependencies within a batch:
--
-- @
-- (pm, ifaces) <- 'await' $ do
--   pm     <- 'current' GetParsedModule file
--   ifaces <- traverse ('current' GetModIface) (importsOf pm)
--   pure (pm, ifaces)
-- @
--
-- If your rule can do without a dependency, you can wrap a read with 'optionally':
--
-- @
-- (ast, docs) <- 'use' $ (,)
--   \<$\> 'optionally' ('current' GetHieAst file)
--   \<*\> 'optionally' ('untrack' \<$\> 'lastKnown' GetDocMap file)
-- @
--
-- For code actions where a user is expected to wait a little, use 'await' to
-- dispatch reads:
--
-- @
-- pm <- 'runQuery' state $ 'await' $ 'current' GetParsedModule file
-- @
--
-- For immediate non-blocking rule demands, use 'peek' to read last known values.
-- These can be a few edits behind. Use 'answerAt' to rewind the cursor to their version
-- and move the answer forward:
--
-- @
-- (ast, env) <- 'peek' $
--   (,) \<$\> 'lastKnown' GetHieAst file \<*\> ('untrack' \<$\> 'lastKnown' GhcSession file)
-- pure $ 'answerAt' (hoverAt (hscEnv env)) cursor ast
-- @
--
-- If you have a list of positions, like tokens, you can 'fastForwardEach' to filter
-- for up to date ones:
--
-- @
-- tokens <- 'peek' ('lastKnown' GetSemanticTokens file)
-- pure ('fastForwardEach' (rangeSemanticList \<$\> tokens))
-- @
--
-- If your plugin returns its own record with ranges, you can derive 'Remappable'
-- for it:
--
-- @
-- data Field = Field
--   { fieldRange  :: Range
--   , fieldLabel  :: Text
--   , fieldName   :: Name
--   }
--   deriving stock Generic
--   deriving anyclass 'Remappable'
-- @
--
-- If you need names, you can bring them out with 'answerAt' too. When the user edited
-- a name's definition, the name only loses its location:
--
-- @
-- let names = fromMaybe [] ('answerAt' getNamesAtPoint cursor ast)
-- @
--
-- A key with a global value can be read with 'Project':
--
-- @
-- targets <- 'use' ('current' GetKnownTargets 'Project')
-- @
module Development.IDE.Core.Use
  ( -- * Reads
    type Freshness (..)
  , Reads
  , current
  , lastKnown
  , optionally
  , Key (Scope)
  , Project (..)
    -- * Runners
  , use
  , await
  , peek
  , Query
  , runQuery
  , MonadPluginFail (..)
    -- * Positions
  , Tracked
  , rewind
  , fastForward
  , fastForwardEach
  , answerAt
  , untrack
  , Remap
  , Remappable (..)
  , Positionless (..)
  , Ageless
  ) where

import           Control.Exception                     (throwIO, try)
import           Control.Monad                         ((>=>))
import           Control.Monad.Extra                   (eitherM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Text                             as T
import           Development.IDE.Core.Internal.Reads
import           Development.IDE.Core.Internal.Shake   (IdeState, Query (..),
                                                        QueryFailed (..),
                                                        enqueue, shakeExtras,
                                                        wait)
import           Development.IDE.Core.Internal.Tracked
import           Development.IDE.Core.Internal.Types
import           Development.IDE.Graph                 (Action)
import           Ide.Plugin.Error

-- | Reads in a rule. The rule depends on every key that the read reaches,
-- missing ones included, so it reruns once they recover.
--
-- Use this in a rule, and 'await' or 'peek' in a handler.
use :: Reads f a -> Action a
use = build >=> either (liftIO . throwIO . BadDependency . show) pure

-- | Waits for a build in a handler. The rounds run as one action, so a
-- restart reruns them against one version.
--
-- Use this when the user can wait a little, e.g. for a code action.
await :: Reads f a -> Query a
await r = do
  pending <- enqueue "await" (build r)
  wait pending >>= either (pluginFailed . ruleFailed) pure

-- | Queues a build of the keys, and only waits for cold keys.
--
-- Use this when the handler has to answer right away, e.g. for a hover or a
-- completion.
peek :: Reads Known a -> Query a
peek r = do
  _ <- enqueue "peek" (build r)
  let go = readLast r >>= \case
        Ready a -> pure a
        Missing keys -> pluginFailed (ruleFailed keys)
        Cold keys -> enqueue ("cold " <> show keys) (needKeys keys) >>= wait >> go
  go

ruleFailed :: [SomeKey] -> PluginError
ruleFailed keys = PluginRuleFailed $ T.intercalate ", " [T.pack (show k) | SomeKey k _ <- keys]

runQuery :: (MonadIO m, MonadPluginFail m) => IdeState -> Query a -> m a
runQuery ide q =
  eitherM (\(QueryFailed e) -> pluginFailed e) pure
    $ liftIO (try (runReaderT (runQueryT q) (shakeExtras ide)))
