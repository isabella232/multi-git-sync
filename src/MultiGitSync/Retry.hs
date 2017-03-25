module MultiGitSync.Retry
  ( retryWithBackoff
  , retryWithInfiniteBackoff
  , exponentialBackoff
  , exponentialBackoffWithFullJitter
  , capStream
  , fullJitter
  ) where

import Protolude

import Control.Monad.Random (MonadRandom(..))

type Microseconds = Int

-- | Exponential backoff with full jitter, capped to the given value.
exponentialBackoffWithFullJitter :: MonadRandom m => Microseconds -> Microseconds -> [m Microseconds]
exponentialBackoffWithFullJitter baseInterval maxInterval =
  fullJitter . capStream maxInterval . exponentialBackoff $ baseInterval

-- | A stream of exponentially backed off microseconds.
exponentialBackoff :: Microseconds -> [Microseconds]
exponentialBackoff start = start : exponentialBackoff (2 * start)

-- | Cap a stream of monotonically-increasing microseconds.
capStream :: Microseconds -> [Microseconds] -> [Microseconds]
capStream limit xs = takeWhile (<limit) xs <> repeat limit

-- | "Full jitter"
--
-- Modulate a random wait period to some random positive number less than it.
fullJitter :: MonadRandom m => [Microseconds] -> [m Microseconds]
fullJitter = map (curry getRandomR 0)


-- | Retry an action until it succeeds. If the source of sleep intervals runs
-- out, we just repeat the last one over and over.
retryWithInfiniteBackoff :: (Show e, MonadIO m) => [m Microseconds] -> m (Either e a) -> m a
retryWithInfiniteBackoff sleeps action = do
  result <- retryWithBackoff (infiniteSleeps sleeps) action
  case result of
    Left err -> panic $ "Should have had infinite sleeps, but ran out and got error: " <> show err
    Right r -> pure r

  where
    infiniteSleeps [x] = repeat x
    infiniteSleeps (x:xs) = x:infiniteSleeps xs
    infiniteSleeps [] = panic "Should never hit empty list on infiniteSleeps"


-- | Retry an action until it succeeds or we run out of retry intervals.
retryWithBackoff :: (MonadIO m) => [m Microseconds] -> m (Either e a) -> m (Either e a)
retryWithBackoff sleeps action =
  loop sleeps
  where
    loop [] = action
    loop (getSleep:rest) = do
      result <- action
      case result of
        Left _ -> do
          sleep <- getSleep
          liftIO $ threadDelay sleep
          loop rest
        _ -> pure result
