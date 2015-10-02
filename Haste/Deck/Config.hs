module Haste.Deck.Config (
    Config, def,
    startAtSlide, transition, onSlideChange
  ) where
import Data.Default
import Haste.DOM (Elem)
import Haste.Deck.Transitions

-- | Configuration for various aspects of decks.
data Config = Config {
    -- | Start presentation at this slide, clamped to @[0, last_slide]@.
    --   Default: @0@
    startAtSlide  :: Int,

    -- | Transition to use when changing slides.
    --   Default: 'none'
    transition    :: Transition,

    -- | Callback to be executed whenever the slide changes. This callback
    --   will fire *after* the slide change animation has completed.
    --   Default: @\_ _ -> return ()@
    onSlideChange :: Int -> Elem -> IO ()
  }

instance Default Config where
  def = Config {
      startAtSlide  = 0,
      transition    = none,
      onSlideChange = \_ _ -> return ()
    }
