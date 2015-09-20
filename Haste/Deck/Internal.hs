module Haste.Deck.Internal (createDeck, enableDeck, disableDeck, toElem) where
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Haste.Concurrent hiding (wait)
import Haste (toString)
import Haste.DOM
import Haste.Events
import Haste.Deck.Types
import Haste.Deck.Transitions
import Haste.Graphics.AnimationFrame
import Haste.Performance

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
createDeck :: MonadIO m => Transition -> [Slide] -> m Deck
createDeck t s = liftIO $ do
    inner <- newElem "div" `with` [style "width" =: "100%",
                                   style "height" =: "100%",
                                   style "position" =: "absolute"]
    e <- newElem "div" `with` [children [inner],
                               style "overflow" =: "hidden",
                               style "padding" =: "0px",
                               style "margin" =: "0px",
                               style "top" =: "0px",
                               style "bottom" =: "0px",
                               style "left" =: "0px",
                               style "right" =: "0px",
                               style "width" =: "auto",
                               style "height" =: "auto",
                               style "position" =: "absolute",
                               style "display" =: "block"]
    v <- newEmptyMVar
    s' <- mapM toElem s
    when (not $ null s') $ do
      concurrent . fork $ do
        setChildren inner (take 1 s')
        go e inner (waitMove v) [] s'
    Deck e v `fmap` newIORef Nothing
  where
    go parent inner wait prev next@(x:xs) = do
      n <- wait
      let (prev', next'@(x':_), changed) =
            case n of
              Next | not (null xs)   -> (x:prev, xs, True)
              Prev | not (null prev) -> (drop 1 prev, head prev:next, True)
              _                      -> (prev, next, False)
      if changed
        then liftIO $ changeSlide n parent inner wait prev' next' x'
        else go parent inner wait prev next
    go _ _ _ _ _ = do
      error "Shouldn't get here!"

    -- Animate the transition from an old one to a new
    changeSlide dir parent inner wait prev next new = do
      t0 <- now
      let duration = transitionDuration t
      newinner <- newElem "div" `with` [style "width" =: "100%",
                                        style "height" =: "100%",
                                        style "position" =: "absolute",
                                        children [new]]
      let animate =
            \t1 -> do
              let progress = min ((t1-t0)/duration) 1
              transitionStep t progress dir parent inner newinner
              if progress < 1
                then do
                  void $ requestAnimationFrame animate
                else do
                  transitionFinished t dir parent inner newinner
                  setChildren newinner [new]
                  setChildren parent [newinner]
                  concurrent $ go parent newinner wait prev next

      void . requestAnimationFrame $ \t1 -> do
        transitionSetup t dir parent inner newinner
        animate t1

-- | Wait for an event that moves the slideshow forward or backward.
waitMove :: MVar KeyData -> CIO NextSlide
waitMove v = do
  x <- takeMVar v
  case x of
    37 -> return Prev -- left
    39 -> return Next -- right
    33 -> return Prev -- pgup
    34 -> return Next -- pgdn
    _  -> waitMove v

data RowOrCol = MkRow | MkCol

-- | Create an element containing a row or a column of sub-elements.
--   Pass width/left/height to create a row, or height/top/width
rowOrCol :: RowOrCol -> [Slide] -> IO Elem
rowOrCol what xs = do
    es <- flip (flip zipWithM xs) [0, step ..] $ \x pos -> do
      e <- toElem x
      newElem "div" `with` [children [e],
                            othersz          =: "100%",
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
    (sizeattr, posattr, othersz) =
      case what of
        MkRow -> (style "width", style "left", style "height")
        MkCol -> (style "height", style "top", style "width")

-- | Compile a slide into a DOM element.
toElem :: Slide -> IO Elem
toElem (Lift x)     = x
toElem (Row xs)     = rowOrCol MkRow xs
toElem (Col xs)     = rowOrCol MkCol xs
toElem (Style s x)  = toElem x `with` s
toElem (PStyle s x) = do
  e <- toElem x
  newElem "div" `with` ([children [e],
                         style "width" =: "100%",
                         style "height" =: "100%",
                         style "position" =: "absolute"] ++ s)
