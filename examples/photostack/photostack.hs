{-# LANGUAGE OverloadedStrings #-}
import Haste.Deck
import Haste.Deck.Extras

main :: IO ()
main = present_ def $ photoStack
  [ "peterrabbit.jpg"
  , "darjeeling.png"
  , "haskell.png"
  , "peterrabbit2.jpg"
  , "darjeeling2.jpg"
  , "haskell2.png"
  , "rainbowdash.jpg"
  ]
