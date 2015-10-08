module Haste.Deck.Types (Slide (..), Deck (..), Proceed (..)) where
import Data.Array
import Data.IORef
import Haste.Concurrent hiding (wait)
import Haste.DOM

-- | A somewhat restricted DOM/CSS representation of a partial web page,
--   specifically geared towards creating presentations and slide shows.
data Slide
  = Row     ![Slide]
  | Col     ![Slide]
  | Style   ![Attribute] !Slide
  | PStyle  ![Attribute] !Slide
  | Lift    !(IO Elem)
  | SizeReq !Double !Slide

-- | A deck of slides.
data Deck = Deck {
    -- | Container element
    deckContainer    :: !Elem,

    deckSlides       :: !(Array Int Elem),

    -- | MVar to write to when a slide change event occurs.
    deckProceedMVar  :: !(MVar Proceed),

    -- | Action to unregister all handlers for the deck, if installed.
    deckUnregHandler :: !(IORef (Maybe (IO ())))
  }

instance IsElem Deck where
  elemOf = deckContainer

-- | Which slide should we proceed to?
data Proceed = Next | Prev | Goto Int | Skip Int
  deriving Eq
