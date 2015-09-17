module Haste.Deck.Internal (createDeck, enableDeck, disableDeck) where
import Control.Monad.State
import Data.IORef
import Haste.Concurrent hiding (wait)
import Haste.DOM
import Haste.Events
import Haste.Deck.Types

-- | Hook keydown events for left, right, pgup and pgdn and use them to flip
--   between the slides of the given deck.
enableDeck :: MonadIO m => Deck -> m ()
enableDeck d = liftIO $ do
  -- IORefs are fine here since there's no concurrency to worry about
  oldhandler <- readIORef (deckKeyHandler d)
  case oldhandler of
    Just _  -> return ()
    Nothing -> do
      h <- documentBody `onEvent` KeyDown $ concurrent . putMVar (deckKeyMVar d)
      writeIORef (deckKeyHandler d) (Just h)

-- | Stop catching keydown events for the given deck.
disableDeck :: MonadIO m => Deck -> m ()
disableDeck d = liftIO $ do
  -- IORefs are fine here since there's no concurrency to worry about
  oldhandler <- readIORef (deckKeyHandler d)
  case oldhandler of
    Just h  -> unregisterHandler h >> writeIORef (deckKeyHandler d) Nothing
    Nothing -> return ()

-- | Create a deck of slides.
createDeck :: MonadIO m => [Slide ()] -> m Deck
createDeck s = liftIO $ do
    e <- newElem "div"
    v <- newEmptyMVar
    concurrent . fork $ go e (waitMove v) [] s
    Deck e v <$> newIORef Nothing
  where
    go parent wait prev next@(x:xs) = do
      slide <- liftIO $ renderSlide x
      setChildren parent [slide]
      n <- wait
      case n of
        Next | not (null xs)   -> go parent wait (x:prev) xs
             | otherwise       -> go parent wait prev next
        Prev | not (null prev) -> go parent wait (drop 1 prev) (head prev:next)
             | otherwise       -> go parent wait prev next
    go _ _ _ _ = do
      error "Shouldn't get here!"

data Next = Next | Prev

waitMove :: MVar KeyData -> CIO Next
waitMove v = do
  x <- takeMVar v
  case x of
    37 -> return Prev -- left
    39 -> return Next -- right
    33 -> return Prev -- pgup
    34 -> return Next -- pgdn
    _  -> waitMove v

renderSlide :: Slide a -> IO Elem
renderSlide (Slide s) = do
  (_, e) <- runStateT s documentBody
  newElem "div" `with` [children [e]]
