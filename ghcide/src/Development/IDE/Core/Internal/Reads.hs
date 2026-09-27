{-# LANGUAGE GADTs    #-}
{-# LANGUAGE TypeData #-}
-- | Batched reads of rule results.
--
-- Similar idea to Haxl, where we batch rule demands and add synchronization
-- points if there are data dependencies.
module Development.IDE.Core.Internal.Reads
  ( -- * Reads
    type Freshness (..)
  , Reads
  , current
  , lastKnown
  , optionally
  , currentAt
  , lastKnownAt
  , SomeKey (..)
    -- * Core
  , build
  , LastKnown (..)
  , readLast
  , needKeys
  ) where

import           Control.Monad                         (unless, void)
import           Control.Monad.IO.Class
import           Control.Monad.Reader                  (ask)
import           Data.Foldable                         (for_, traverse_)
import           Data.Functor                          ((<&>))
import qualified Data.HashMap.Strict                   as HashMap
import           Data.IORef
import           Data.List.NonEmpty                    (NonEmpty (..))
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (isJust, mapMaybe)
import           Data.Type.Equality                    ((:~:) (..))
import           Data.Typeable                         (TypeRep, eqT, typeOf)
import           Development.IDE.Core.Internal.Shake
import           Development.IDE.Core.Internal.Tracked (Tracked, tracked)
import           Development.IDE.Core.Internal.Types
import           Development.IDE.Graph                 (Action, RuleResult,
                                                        parallel)
import           Development.IDE.Graph.Rule            (apply)
import           Development.IDE.Types.Location        (NormalizedFilePath)
import           Development.IDE.Types.Shake           (A (..), currentValue)

-- | Tag reads with whether they can wait for a build of the current version, or
-- are capable of working with already known outdated values.
type data Freshness = Current | Known

newtype Reads (f :: Freshness) a = Reads
  { step :: IO (Step f a)
  }

data Step f a
  = Done a
  | Failed [SomeKey]
  | Blocked [Request f] (Reads f a)

instance Functor (Reads f) where
  fmap f (Reads r) = Reads $ r <&> \case
    Done a -> Done (f a)
    Failed ks -> Failed ks
    Blocked rs next -> Blocked rs (fmap f next)

instance Applicative (Reads f) where
  pure a = Reads $ pure (Done a)
  Reads rf <*> Reads ra = Reads $ do
    sf <- rf
    sa <- ra
    pure $ case (sf, sa) of
      (Done f, Done a)             -> Done (f a)
      (Done f, Blocked rs a)       -> Blocked rs (f <$> a)
      (Blocked rs f, Done a)       -> Blocked rs (($ a) <$> f)
      (Blocked rs f, Blocked qs a) -> Blocked (rs <> qs) (f <*> a)
      (Failed ks, Failed ls)       -> Failed (ks <> ls)
      (Failed ks, _)               -> Failed ks
      (_, Failed ls)               -> Failed ls

instance Monad (Reads f) where
  Reads r >>= k = Reads $ r >>= \case
    Done a -> step (k a)
    Failed ks -> pure (Failed ks)
    Blocked rs next -> pure (Blocked rs (next >>= k))

current :: forall k. Key k => k -> Scope k -> Reads Current (RuleResult k)
current k s = currentAt k (scopeFile s)

-- | The last value that succeeded, which can be of an older version.
--
-- Use this when an older value will do, e.g. to answer a hover while the user
-- types.
lastKnown :: forall k f. Key k => k -> Scope k -> Reads f (Tracked (RuleResult k))
lastKnown k s = lastKnownAt k (scopeFile s)

-- | A missing key gives 'Nothing'.
--
-- Use this when the caller can do without the value.
optionally :: Reads f a -> Reads f (Maybe a)
optionally (Reads r) = Reads $ r <&> \case
  Done a -> Done (Just a)
  Failed _ -> Done Nothing
  Blocked rs next -> Blocked rs (optionally next)

-- | Does the same as 'current', for a key without a 'Key' instance.
currentAt :: IdeRule k (RuleResult k) => k -> NormalizedFilePath -> Reads Current (RuleResult k)
currentAt = request WantCurrent

-- | Does the same as 'lastKnown', for a key without a 'Key' instance.
lastKnownAt :: IdeRule k (RuleResult k) => k -> NormalizedFilePath -> Reads f (Tracked (RuleResult k))
lastKnownAt = request WantLast

data Want (f :: Freshness) k r where
  WantCurrent :: Want Current k (RuleResult k)
  WantLast :: Want f k (Tracked (RuleResult k))

data Request f where
  Request
    :: IdeRule k (RuleResult k)
    => Want f k r -> k -> NormalizedFilePath -> IORef (Maybe r) -> Request f

data SomeKey where
  SomeKey :: IdeRule k (RuleResult k) => k -> NormalizedFilePath -> SomeKey

instance Show SomeKey where
  show (SomeKey k file) = show (Q (k, file))

request :: IdeRule k (RuleResult k) => Want f k r -> k -> NormalizedFilePath -> Reads f r
request want k file = Reads $ do
  slot <- newIORef Nothing
  pure $ Blocked [Request want k file slot] $ Reads $
    maybe (Failed [SomeKey k file]) Done <$> readIORef slot

runRounds :: MonadIO m => ([Request f] -> m ()) -> Reads f a -> m (Either [SomeKey] a)
runRounds answer (Reads r) = liftIO r >>= \case
  Done a -> pure (Right a)
  Failed ks -> pure (Left ks)
  Blocked rs next -> answer rs >> runRounds answer next

-- | Each round runs one 'apply' per type of key. 'Left' lists the missing keys of
-- the round that failed.
build :: Reads f a -> Action (Either [SomeKey] a)
build = runRounds buildRound

buildRound :: forall f. [Request f] -> Action ()
buildRound requests = do
  extras <- getShakeExtras
  -- A part that throws loses the dependencies of every part, so each part
  -- leaves its missing keys in the slots.
  void $ parallel (applyGroup extras <$> Map.elems groups)
  where
    groups :: Map.Map TypeRep (NonEmpty (Request f))
    groups = Map.fromListWith (<>) [(keyType r, pure r) | r <- requests]
    keyType (Request _ k _ _) = typeOf k

data Typed f k where
  Typed :: Want f k r -> k -> NormalizedFilePath -> IORef (Maybe r) -> Typed f k

applyGroup :: ShakeExtras -> NonEmpty (Request f) -> Action ()
applyGroup extras (Request want0 k0 file0 slot0 :| rest) =
  applyTyped (Typed want0 k0 file0 slot0 : mapMaybe (asTyped k0) rest)
  where
    applyTyped typed = do
      let (keys, sharing) = unzip $ HashMap.toList $
            HashMap.fromListWith (<>) [(Q (k, file), [t]) | t@(Typed _ k file _) <- typed]
      results <- apply keys
      liftIO $ for_ (zip sharing results) $ \(ts, A value) ->
        for_ ts $ \(Typed want k file slot) -> writeIORef slot =<< case want of
          WantCurrent -> pure (currentValue value)
          WantLast    -> fmap (tracked file) <$> lastValueIO extras k file

asTyped :: forall k f. IdeRule k (RuleResult k) => k -> Request f -> Maybe (Typed f k)
asTyped _ (Request want (k :: k') file slot) = case eqT @k @k' of
  Just Refl -> Just (Typed want k file slot)
  Nothing   -> Nothing

data LastKnown a
  = Ready a
  | Missing [SomeKey]
  -- | Keys that never ran in this session, and have no persisted value.
  | Cold [SomeKey]

-- | Reads the last known values without waiting or touching the build.
readLast :: Reads Known a -> Query (LastKnown a)
readLast r = do
  extras <- ask
  coldRef <- liftIO $ newIORef []
  outcome <- runRounds (readRound extras coldRef) r
  cold <- liftIO $ readIORef coldRef
  pure $ case (cold, outcome) of
    (_ : _, _)    -> Cold cold
    ([], Left ks) -> Missing ks
    ([], Right a) -> Ready a

readRound :: ShakeExtras -> IORef [SomeKey] -> [Request Known] -> Query ()
readRound extras cold requests = for_ requests $ \(Request WantLast k file slot) -> do
  -- Checked before the value, so a build that ends in between is not cold.
  ran <- hasRun k file
  value <- liftIO $ peekLastValue extras k file
  liftIO $ writeIORef slot (tracked file <$> value)
  unless (ran || isJust value) $ liftIO $ modifyIORef' cold (SomeKey k file :)

-- | Builds the keys, whether or not they are missing.
needKeys :: [SomeKey] -> Action ()
needKeys keys = void (build (traverse_ need keys))
  where need (SomeKey k file) = void (optionally (currentAt k file))
