module Haste.Deck.Internal (createDeck, enableDeck, disableDeck, toElem) where
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Haste.Concurrent hiding (wait)
import Haste (toString)
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
createDeck :: MonadIO m => [Slide] -> m Deck
createDeck s = liftIO $ do
    e <- newElem "div" `with` [style "overflow" =: "hidden"]
    v <- newEmptyMVar
    s' <- mapM toElem s
    concurrent . fork $ go e (waitMove v) [] s'
    Deck e v <$> newIORef Nothing
  where
    go parent wait prev next@(x:xs) = do
      setChildren parent [x]
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

rowOrCol :: AttrName -> AttrName -> AttrName -> [Slide] -> IO Elem
rowOrCol sizeattr posattr othersize xs = do
    es <- flip (flip zipWithM xs) [0, step ..] $ \x pos -> do
      e <- toElem x
      newElem "div" `with` [children [e],
                            othersize        =: "100%",
                            sizeattr         =: stepStr,
                            posattr          =: (toString pos ++ "%"),
                            style "position" =: "absolute"]
    newElem "div" `with` [children es,
                          style "position" =: "absolute",
                          style "width" =: "100%",
                          style "height" =: "100%"]
  where
    step = 100 / fromIntegral (length xs) :: Double
    stepStr = show step ++ "%"


toElem :: Slide -> IO Elem
toElem (Lift x)     = x
toElem (Row xs)     = rowOrCol (style "width") (style "left") (style "height") xs
toElem (Col xs)     = rowOrCol (style "height") (style "top") (style "width") xs
toElem (Style s x)  = toElem x `with` s
toElem (PStyle s x) = do
  e <- toElem x
  newElem "div" `with` ([children [e],
                         style "width" =: "100%",
                         style "height" =: "100%",
                         style "position" =: "absolute"] ++ s)
toElem (Group x)    = do
  e <- toElem x
  newElem "div" `with` [children [e],
                        style "width" =: "100%",
                        style "height" =: "100%",
                        style "position" =: "absolute"]
