{-# LANGUAGE OverloadedStrings #-}
-- | Runner functions with lots of internal, nasty IO/DOM/CSS stuff.
module Haste.Deck.Internal (createDeck, toElem) where
import Control.Monad
import Control.Monad.IO.Class
import Data.Array
import Data.IORef
import Haste (toJSString)
import Haste.Concurrent hiding (wait, (!))
import Haste.DOM.JSString
import Haste.Deck.Config
import Haste.Deck.Types
import Haste.Deck.Transitions
import Haste.Graphics.AnimationFrame
import Haste.Performance
import qualified Haste.JSString as J

-- | Create a deck of slides.
createDeck :: MonadIO m => Config -> [Slide] -> m Deck
createDeck cfg s = liftIO $ do
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
    let len    = length s'
        slides = listArray (1, len) s'
        ix     = max 1 (min len (startAtSlide cfg))
    unless (null s') $ concurrent . fork $ do
      setChildren inner [slides ! ix]
      start e (takeMVar v) len slides inner ix
    Deck e slides v `fmap` newIORef Nothing
  where
    t = transition cfg

    start parent wait len slides = go
      where
        clamp = max 1 . min len
        go inner ix = do
          proceed <- wait
          let ix' =
               clamp $ case proceed of
                         Next   -> ix+1
                         Prev   -> ix-1
                         Goto n -> n
                         Skip n -> ix + n
          if ix /= ix'
            then liftIO $ changeSlide inner (slides ! ix') ix ix'
            else go inner ix

        -- Animate the transition from an old one to a new
        changeSlide :: Elem -> Elem -> Int -> Int -> IO ()
        changeSlide inner new ix ix' = do
          t0 <- now
          let duration = transitionDuration t
          newinner <- newElem "div" `with` [style "width" =: "100%",
                                            style "height" =: "100%",
                                            style "position" =: "absolute",
                                            children [new]]
          let animate t1 = do
                  let progress = min ((t1-t0)/duration) 1
                  transitionStep t progress ix ix' parent inner newinner
                  if progress < 1
                    then void $ requestAnimationFrame animate
                    else do
                      transitionFinished t ix ix' parent inner newinner
                      setChildren newinner [new]
                      setChildren parent [newinner]
                      onSlideChange cfg ix ix'
                      concurrent $ go newinner ix'

          void . requestAnimationFrame $ \t1 -> do
            transitionSetup t ix ix' parent inner newinner
            animate t1

data RowOrCol = MkRow | MkCol

-- | Create an element containing a row or a column of sub-elements.
--   Pass width/left/height to create a row, or height/top/width
rowOrCol :: RowOrCol -> Double -> [Slide] -> IO Elem
rowOrCol what unalloc xs = do
    parent <- newElem "div" `with` [style "position" =: "absolute",
                                    style "width" =: "100%",
                                    style "height" =: "100%"]

    revFoldM 0 xs $ \pos x -> do
      let (pos', szstr) = case x of
                            SizeReq r _ -> (pos + r, pctstr r)
                            _           -> (pos + step, stepStr)
          posstr = J.snoc (toJSString (pos*100)) '%'
      e <- toElem x
      e' <- newElem "div" `with` [children [e],
                                  othersz          =: "100%",
                                  sizeattr         =: szstr,
                                  posattr          =: posstr,
                                  style "position" =: "absolute"]
      appendChild parent e'
      return pos'

    return parent
  where
    step = unalloc / fromIntegral (length xs - length [0::Int | SizeReq _ _ <- xs])
    stepStr = J.snoc (toJSString (step*100)) '%'
    pctstr frac = J.snoc (toJSString (frac*100)) '%'
    (sizeattr, posattr, othersz) =
      case what of
        MkRow -> (style "width", style "left", style "height")
        MkCol -> (style "height", style "top", style "width")
    revFoldM acc es f = foldM_ f acc es

-- | Calculate the space available for slides that haven't requested a certain
--   percentage of the screen.
unallocatedSpace :: [Slide] -> Double
unallocatedSpace = max 0 . (1-) . sum . map sizeReq
  where
    sizeReq (SizeReq r _) = r
    sizeReq _             = 0

-- | Compile a slide into a DOM element.
toElem :: Slide -> IO Elem
toElem (Lift x)      = x
toElem (Row xs)      = rowOrCol MkRow (unallocatedSpace xs) xs
toElem (Col xs)      = rowOrCol MkCol (unallocatedSpace xs) xs
toElem (Style s x)   = toElem x `with` s
toElem (SizeReq _ x) = toElem x
toElem (PStyle s x)  = do
  e <- toElem x
  newElem "div" `with` ([children [e],
                         style "width" =: "100%",
                         style "height" =: "100%",
                         style "position" =: "absolute"] ++ s)
