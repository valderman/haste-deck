-- | Controls and helpers for changing slides in a deck.
module Haste.Deck.Control (
    forward, back, goto, skip,
    present, enableDeck, disableDeck
  ) where
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Haste
import Haste.Concurrent
import Haste.DOM
import Haste.Events
import Haste.Deck.Config
import Haste.Deck.Internal
import Haste.Deck.Types

-- | Advance a deck to the next slide.
forward :: MonadIO m => Deck -> m ()
forward = liftIO . concurrent . flip putMVar Next . deckProceedMVar

-- | Go back one slide.
back :: MonadIO m => Deck -> m ()
back = liftIO . concurrent . flip putMVar Prev . deckProceedMVar

-- | Go to the given slide. Clamped to @[0, num_slides-1]@.
goto :: MonadIO m => Deck -> Int -> m ()
goto d = liftIO . concurrent . putMVar (deckProceedMVar d) . Goto

-- | Skip a number of slides. Negative numbers skip backward.
skip :: MonadIO m => Deck -> Int -> m ()
skip d = liftIO . concurrent . putMVar (deckProceedMVar d) . Skip

-- | Hook keydown events for left and right, pgup and pgdn and use them to flip
--   between the slides of the given deck.
--   Also hooks home and end to skip to the first and last slide respectively.
--
--   Note that it is not necessary to call @enableDeck@ in order to animate
--   a deck; it is a mere convenience function. For more fine-grained control,
--   'forward', 'back', etc. can be used to change slides on custom triggers.
enableDeck :: MonadIO m => Deck -> m ()
enableDeck d = liftIO $ do
  -- IORefs are fine here since there's no concurrency to worry about
  oldhandler <- readIORef (deckKeyHandler d)
  case oldhandler of
    Just _  -> return ()
    Nothing -> do
      h <- documentBody `onEvent` KeyDown $ \key -> do
        case key of
          37 -> back d          -- left
          39 -> forward d       -- right
          33 -> back d          -- pgup
          34 -> forward d       -- pgdn
          36 -> goto d 0        -- home
          35 -> goto d maxBound -- end
          _  -> return ()
      writeIORef (deckKeyHandler d) (Just h)

-- | Stop catching keydown events for the given deck.
disableDeck :: MonadIO m => Deck -> m ()
disableDeck d = liftIO $ do
  -- IORefs are fine here since there's no concurrency to worry about
  oldhandler <- readIORef (deckKeyHandler d)
  case oldhandler of
    Just h  -> unregisterHandler h >> writeIORef (deckKeyHandler d) Nothing
    Nothing -> return ()

-- | Run a deck in "presenter mode" - display slides in full screen, hook the
--   arrow and page up/down keys for navigation, and inspect the hash part of
--   the URL for which slide to start at.
present :: Config -> [Slide] -> IO Deck
present cfg s = do
    slideNo <- maybe (0 :: Int) id . fromString <$> getHash
    r <- newIORef True
    d <- flip createDeck s $ cfg {startAtSlide = slideNo,
                                  onSlideChange = \_ n -> safeSetHash r (show n)}
    onHashChange $ \_ h -> do
      enable <- readIORef r
      when enable $ do
        case fromString h of
          Just n -> goto d n
          _      -> goto d 0
    setChildren documentBody [d]
    enableDeck d
    return d
  where
    -- Hack to work around the fact that setting the hash triggers the
    -- hashchange event.
    safeSetHash r hash = do
      writeIORef r False
      setHash hash
      void $ setTimer (Once 100) $ writeIORef r True
