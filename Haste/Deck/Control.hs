-- | Controls and helpers for changing slides in a deck.
module Haste.Deck.Control (
    forward, back, goto, skip,
    present, present_, enableDeck, disableDeck
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

-- | Go to the given slide. Clamped to @[1, num_slides]@.
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
  oldhandler <- readIORef (deckUnregHandler d)
  case oldhandler of
    Just _  -> return ()
    Nothing -> do
      h <- documentBody `onEvent` KeyDown $ \key ->
        case key of
          37 -> back d          -- left
          39 -> forward d       -- right
          33 -> back d          -- pgup
          34 -> forward d       -- pgdn
          36 -> goto d 0        -- home
          35 -> goto d maxBound -- end
          _  -> return ()
      unswipe <- onSwipe $ \dir ->
        case dir of
          L -> forward d
          R -> back d
      writeIORef (deckUnregHandler d) (Just $ unregisterHandler h >> unswipe)

-- | Stop catching keydown events for the given deck.
disableDeck :: MonadIO m => Deck -> m ()
disableDeck d = liftIO $ do
  -- IORefs are fine here since there's no concurrency to worry about
  munreg <- readIORef (deckUnregHandler d)
  case munreg of
    Just unreg -> unreg >> writeIORef (deckUnregHandler d) Nothing
    Nothing    -> return ()

-- | Run a deck in "presenter mode" - display slides in full screen, hook the
--   keys and touch events used by 'enableDeck' for navigation, and inspect
--   the hash part of the URL for which slide to start at.
present :: Config -> [Slide] -> IO Deck
present cfg s = do
    hash <- getHash
    let setSlideNo = case fromJSString hash of
                       Just ix | ix > 0 -> const ix
                       _                -> id
    r <- newIORef True
    d <- flip createDeck s $ cfg {startAtSlide = setSlideNo $ startAtSlide cfg,
                                  onSlideChange = \_ n -> safeSetHash r n}
    onHashChange $ \_ h -> do
      enable <- readIORef r
      when enable $ case fromJSString h of
                      Just n -> goto d n
                      _      -> goto d 0
    setChildren documentBody [d]
    enableDeck d
    return d
  where
    -- Hack to work around the fact that setting the hash triggers the
    -- hashchange event.
    safeSetHash r slide = do
      writeIORef r False
      setHash (toJSString slide)
      void $ setTimer (Once 100) $ writeIORef r True

-- | Run presenter mode without returning the deck.
present_ :: Config -> [Slide] -> IO ()
present_ cfg slides = void $ present cfg slides

data Direction = L | R

-- | Runs an action when a single touch left or right swipe happens.
onSwipe :: (Direction -> IO ()) -> IO (IO ())
onSwipe m = do
  r <- newIORef Nothing

  -- When a touch happens, save the coordinates for later.
  h1 <- documentBody `onEvent` TouchStart $ \td ->
    case changedTouches td of
      [t] -> writeIORef r (Just t)
      _   -> return ()

  -- If the previously registered touch moves, register a swipe if it moves
  -- horizontally by >=30 pixels and vertically by <30 pixels.
  h2 <- documentBody `onEvent` TouchMove $ \td -> do
    mt <- readIORef r
    case (mt, changedTouches td) of
      (Just t, [t']) | identifier t == identifier t' ->
        case (screenCoords t, screenCoords t') of
          ((x, y), (x', y'))
            | abs y - abs y' >= 30  -> return () -- diagonal swipe; ignore
            | x' - x         <= -30 -> writeIORef r Nothing >> m L
            | x' - x         >= 30  -> writeIORef r Nothing >> m R
            | otherwise             -> return ()
      _ ->
        return ()

  -- When the registered touch leaves the screen, unregister it.
  h3 <- documentBody `onEvent` TouchEnd $ \td -> do
    mtouch <- readIORef r
    case fmap identifier mtouch of
      Just ident | any (\t' -> ident == identifier t') (changedTouches td) ->
        writeIORef r Nothing
      _ ->
        return ()
  return $ mapM_ unregisterHandler [h1, h2, h3]
