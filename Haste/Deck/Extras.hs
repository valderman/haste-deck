{-# LANGUAGE OverloadedStrings #-}
-- | Higher level slide combinators, built on top of core Haste.Deck.
module Haste.Deck.Extras
  ( photoStack, photoStackWith
  ) where
import Haste (URL)
import Haste.Deck
import Haste.DOM
import System.Random
import Data.List (foldl')

defaultAttrs :: Slide -> Slide
defaultAttrs = withAttrs
  [ style "border" =: "2px solid black"
  , style "padding" =: "1em"
  , style "height" =: "30em"
  , style "background-color" =: "white"
  ]

-- | Build a photo stack from a list of URLs, using the given random
--   generator and per-photo styling function.
photoStackWith :: StdGen -> (Slide -> Slide) -> [URL] -> [Slide]
photoStackWith gen attrs =
    map column . buildPhotoStackList
  where
    buildPhotoStackList =
      reverse . snd . foldl' (\(g, ps) url -> consPhoto url g ps) (gen, [])

    consPhoto :: URL -> StdGen -> [[Slide]] -> (StdGen, [[Slide]])
    consPhoto url g ps =
      case (ps, rotatedPhoto g url) of
        (prev : _, (g', photo)) -> (g', (prev ++ [photo]) : ps)
        (_, (g', photo))        -> (g', [[photo]])
    
    rotatedPhoto g url =
      case randomR (-45, 45 :: Int) g of
        (n, g') -> (g', photo n url)

    photo deg =
      withAttrs [ style "transform" =: ("rotate(" ++ show deg ++ "deg)")
                , style "margin-top" =: "10em" ]
      . centered
      . attrs
      . image

-- | Build a photo stack from a list of URL's.
--   A photo stack is a list of slides in which the first slide contains one
--   image, the second slide contains another image stacked on top of the first
--   image, the third slide contains a third slide stacked on top of the other
--   two, etc.
--
--   The images are randomly rotated by 45 to -45 degrees.
--   For best results, the images should have similar proportions.
photoStack :: [URL] -> [Slide]
photoStack = photoStackWith (mkStdGen 42) defaultAttrs
