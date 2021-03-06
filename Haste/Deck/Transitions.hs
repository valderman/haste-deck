{-# LANGUAGE OverloadedStrings #-}
module Haste.Deck.Transitions (
    -- * The 'Transition' type
    Progress, Transition, Proceed (..),
    transitionDuration, transitionStep, transitionSetup, transitionFinished,

    -- * Animations
    none, pan, fade, fadeOver, blend
  ) where
import Haste.Deck.Types
import Haste.DOM.JSString
import Haste.JSString (snoc)
import Haste (toJSString)

type Progress = Double

data Transition = Transition {
    -- | Duration of transition animation, in milliseconds.
    transitionDuration :: Double,

    -- | One step of the transition animation.
    transitionStep     :: Progress -- ^ Progress of the animation as a floating
                                   --   point number in the range [0, 1].
                       -> Int      -- ^ Which slide are we coming from?
                       -> Int      -- ^ Which slide are we going to?
                       -> Elem     -- ^ Parent element of slide.
                       -> Elem     -- ^ Element of the old slide.
                       -> Elem     -- ^ Element of the new slide.
                       -> IO (),

    -- | Called once when setting up the transition. Any initialization, such
    --   as adding a new slide to the animation parent, goes here.
    --   Same arguments as 'transitionStep'.
    transitionSetup    :: Int -> Int -> Elem -> Elem -> Elem -> IO (),

    -- | Called once after the transition has finished. Any cleanup, such as
    --   removing animation specific CSS from the parent, goes here.
    --   Same arguments as 'transitionStep'.
    transitionFinished :: Int -> Int -> Elem -> Elem -> Elem -> IO ()
  }

-- | The default transition does nothing for 0 milliseconds.
none :: Transition
none = Transition {
    transitionDuration = 0,
    transitionStep     = \_ _ _ _ _ _ -> return (),
    transitionSetup    = \_ _ _ _ _ -> return (),
    transitionFinished = \_ _ _ _ _ -> return ()
  }

-- | Pan slides from right to left when going forward, and from left to right
--   when going backward.
pan :: Transition
pan = none {
      transitionDuration = 300
    
    , transitionSetup = \from to parent old new ->
        if from > to -- going backwards
          then insertChildBefore parent old new
          else appendChild parent new

    , transitionStep = \progress from to _ old new -> do
        let p = sin ((progress-0.5)*pi)*50+50
            oldpct
              | from > to = toJSString p `snoc` '%'
              | otherwise = toJSString (negate p) `snoc` '%'
            newpct
              | from > to = toJSString (p - 100) `snoc` '%'
              | otherwise = toJSString (100 - p) `snoc` '%'
        set old [style "left" =: oldpct]
        set new [style "left" =: newpct]
    }

-- | Fade the old slide out then fade the new one in.
fade :: Transition
fade = none {
      transitionDuration = 300
    
    , transitionSetup = \_ _ parent _ new -> do
        set new [style "position" =: "absolute",
                 style "top" =: "0px",
                 style "left" =: "0px",
                 style "opacity" =: "0"]
        appendChild parent new

    , transitionStep = \progress _ _ _ old new ->
        if progress < 0.5
          then set old [style "opacity" =: toJSString  (1 - progress*2)]
          else set new [style "opacity" =: toJSString  (progress*2 - 1)]
    }

-- | Fade the old slide out while fading the new one in.
blend :: Transition
blend = fade {
    transitionStep = \progress _ _ _ old new -> do
       set old [style "opacity" =: toJSString  (1 - progress)]
       set new [style "opacity" =: toJSString  progress]
  }

-- | Fade the new slide in on top of the old one. This usually looks best for
--   slides with an opaque background.
fadeOver :: Transition
fadeOver = fade {
    transitionStep = \progress _ _ _ _ new ->
      set new [style "opacity" =: toJSString progress]
  }
