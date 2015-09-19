{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Haste.Deck where
import Control.Monad
import Data.List
import Haste
import Haste.DOM
import Haste.Foreign hiding (get)
import Haste.Deck.Types

data FontSize
  = Pt Int
  | Em Int

instance Show FontSize where
  show (Pt n) = show n ++ "pt"
  show (Em n) = show n ++ "em"  

data ListStyle = Numbered | Unnumbered

instance Show ListStyle where
  show Numbered   = "ol"
  show Unnumbered = "ul"

withAttrs :: [Attribute] -> Slide -> Slide
withAttrs as (Style as' s) = Style (as ++ as') s
withAttrs as (PStyle ps s) = PStyle ps (withAttrs as s)
withAttrs as s             = Style as s

parentStyle :: [Attribute] -> Slide -> Slide
parentStyle as (PStyle as' s) = PStyle (as ++ as') s
parentStyle as s              = PStyle as s

-- | Render a string of text.
text :: String -> Slide
text s = Lift $ newElem "div" `with` ["textContent" =: s]

-- | Render the given slide using the given color.
color :: String -> Slide -> Slide
color c = withAttrs [style "color" =: c]

-- | Display the given slide with the given font size.
fontSize :: FontSize -> Slide -> Slide
fontSize sz = withAttrs [style "fontSize" =: show sz]

-- | Display the given slide with the given font face.
font :: String -> Slide -> Slide
font f = withAttrs [style "font-family" =: f]

-- | Display the given slide with the given CSS class.
withClass :: String -> Slide -> Slide
withClass c = withAttrs ["class" =: c]

-- | Display the given slide centered.
centered :: Slide -> Slide
centered = parentStyle [style "text-align" =: "center"]

-- | Display the given slide centered.
verticallyCentered :: Slide -> Slide
verticallyCentered = mapLeaf $ withAttrs [style "position" =: "relative",
                                          style "top" =: "50%",
                                          style "transform" =: "translateY(-50%)"]

-- | Put the first slide above the second.
above :: Slide -> Slide -> Slide
above (Col as) (Col bs) = Col (as ++ bs)
above (Col as) b        = Col (as ++ [b])
above a        (Col bs) = Col (a : bs)
above a        b        = Col [a, b]

-- | Put the first slide to the left of the second.
leftOf :: Slide -> Slide -> Slide
leftOf (Row ls) (Row rs) = Row (ls ++ rs)
leftOf (Row ls) r        = Row (ls ++ [r])
leftOf l        (Row rs) = Row (l : rs)
leftOf l        r        = Row [l, r]

-- | Create a layout group out of one or more slides. A layout group is used
--   to control how space is split among slides. For example:
--
--   > a `above` b `above` c
--
--   In the above compound slide, @a@, @b@ and @c@ will be composed vertically,
--   taking up 33% of the available vertical space each. We can change this
--   space allotment using @group@:
--
--   > a `above` group (b `above` c)
--
--   In this compound slide, the available vertical space will be split evenly
--   between @a@ on the one hand, and @b@ and @c@ on the other. Thus, @a@ will
--   get 50% of the available space, and @b@ and @c@ will get 25% each.
group :: Slide -> Slide
group = Group

-- | Create a row of slides, with all slides taking up equal space.
row :: [Slide] -> Slide
row = foldl1' (flip leftOf)

-- | Create a column of slides, with all slides taking up equal space.
column :: [Slide] -> Slide
column = foldl1' (flip above)

-- | Render an image.
image :: URL -> Slide
image url = Lift $ newElem "img" `with` ["src" =: url]

list :: ListStyle -> [String] -> Slide
list listtype rows = Lift $ do
  e <- newElem (show listtype)
  forM_ rows $ \r -> do
    newElem "li" `with` ["textContent" =: r] >>= appendChild e
  set e [style "margin" =: "0px",
         style "padding" =: "0px",
         style "position" =: "absolute",
         style "width" =: "100%",
         style "height" =: "100%",
         style "left" =: "0px"]
  newElem "div" `with` [children [e],
                        style "display" =: "inline-block",
                        style "position" =: "absolute",
                        style "width" =: "100%",
                        style "height" =: "100%",
                        style "border" =: "1px solid black",
                        style "transform" =: "translateX(50%)"]
