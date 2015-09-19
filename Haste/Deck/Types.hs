{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Haste.Deck.Types (Slide (..), Deck (..), mapLeaf) where
import Data.IORef
import Haste.Concurrent hiding (wait)
import Haste.DOM
import Haste.Events

data Slide
  = Row    ![Slide]
  | Col    ![Slide]
  | Style  ![Attribute] !Slide
  | PStyle ![Attribute] !Slide
  | Group  !Slide
  | Lift   !(IO Elem)

data Deck = Deck {
    deckContainer  :: !Elem,
    deckKeyMVar    :: !(MVar KeyData),
    deckKeyHandler :: !(IORef (Maybe HandlerInfo))
  }

instance IsElem Deck where
  elemOf = deckContainer

mapLeaf :: (Slide -> Slide) -> Slide -> Slide
mapLeaf f = go
  where
    go (Row xs)      = Row $ map go xs
    go (Col xs)      = Col $ map go xs
    go (Style as x)  = Style as $ go x
    go (PStyle as x) = PStyle as $ go x
    go (Group x)     = Group $ go x
    go x@(Lift _)    = f x
