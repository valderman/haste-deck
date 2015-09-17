{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Haste.Deck.Types (Slide (..), Deck (..), MonadState (..), liftIO) where
import Control.Monad.State
import Data.IORef
import Haste.Concurrent hiding (wait)
import Haste.DOM
import Haste.Events

newtype Slide a = Slide (StateT Elem IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Elem)

data Deck = Deck {
    deckContainer  :: !Elem,
    deckKeyMVar    :: !(MVar KeyData),
    deckKeyHandler :: !(IORef (Maybe HandlerInfo))
  }

instance IsElem Deck where
  elemOf = deckContainer
