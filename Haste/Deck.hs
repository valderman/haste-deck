{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Create slideshows and layout web pages in a compositional manner.
module Haste.Deck (
    -- * Basic types
    Slide, Deck,

    -- * Deck configuration
    Config, def,
    startAtSlide, transition, onSlideChange,

    -- * Custom slide control
    forward, back, goto, skip,

    -- * Building and using slide decks
    createDeck, enableDeck, disableDeck, present, present_, toElem,

    -- * CSS/DOM inclusion
    lift, element, html, withAttrs, groupAttrs, withClass,

    -- * Layout
    sized, leftOf, above, group, row, column,

    -- * Styling
    Markup, Size (..), Alignment (..),
    aligned, centered, verticallyCentered,
    color, textBackground, backgroundColor, fontSize, font, padded,
    defaultTextPadding,
    
    -- * Primitives
    ListStyle (..),
    markup, text, image, list, sublist, code,

    -- * Transitions
    Transition,
    none, pan, fade, fadeOver, blend
  ) where
import Control.Monad
import Data.List hiding (group)
import Data.String
import Haste hiding (fromString)
import Haste.DOM.JSString
import Haste.Foreign
import qualified Haste.JSString as J
import System.IO.Unsafe
import Haste.Deck.Config
import Haste.Deck.Control
import Haste.Deck.Internal
import Haste.Deck.Markup
import Haste.Deck.Transitions
import Haste.Deck.Types

instance IsString Slide where
  fromString = markup . fromString

-- | A font size, expressed in points, elements or pixels.
data Size
  = Pt  Double
  | Em  Double
  | Px  Double
  | Pct Double

sizeString :: Size -> JSString
sizeString (Pt  n) = J.append (toJSString n) "pt"
sizeString (Em  n) = J.append (toJSString n) "em"
sizeString (Px  n) = J.append (toJSString n) "px"
sizeString (Pct n) = J.append (toJSString n) "%"

-- | Slide element alignment. West/East instead of Left/Right to avoid clashing
--   with 'Either'.
data Alignment
  = West
  | Center
  | East

alignString :: Alignment -> JSString
alignString West   = "left"
alignString Center = "center"
alignString East   = "right"

-- | A list may be either numbered or unnumbered.
data ListStyle = Numbered | Unnumbered

listStyleString :: ListStyle -> JSString
listStyleString Numbered = "ol"
listStyleString Unnumbered = "ul"

-- | The default padding for text elements.
textPadding :: JSString
textPadding = sizeString defaultTextPadding

-- | The default padding for text slides.
defaultTextPadding :: Size
defaultTextPadding = Pt 18

-- | Include an arbitrary DOM element in a slide.
--   In order for horizontal alignment to work properly, the element - or
--   the parts of it that should be affected by alignment - should declare
--   @display: inline-block@.
element :: IsElem e => e -> Slide
element = lift . return

-- | Include a dynamically created DOM element in a slide.
--   See 'element' for information about horizontal alignment.
lift :: IsElem e => IO e -> Slide
lift = Lift . fmap elemOf

-- | Include verbatim HTML in a slide.
--   Any elements to be centered should declare @display: inline-block@.
html :: String -> Slide
html s = lift $ newElem "div" `with` ["innerHTML" =: toJSString s]

-- | Apply a list of attributes to the given slide.
withAttrs :: [Attribute] -> Slide -> Slide
withAttrs as (Style as' s) = Style (as ++ as') s
withAttrs as (PStyle ps s) = PStyle ps (withAttrs as s)
withAttrs as s             = Style as s

-- | Apply a list of attributes to the topmost layout group of the given slide.
--   If no such group exists, it is created.
groupAttrs :: [Attribute] -> Slide -> Slide
groupAttrs as (PStyle as' s) = PStyle (as ++ as') s
groupAttrs as s              = PStyle as s

-- | Display the given slide with the given CSS class.
withClass :: String -> Slide -> Slide
withClass c = groupAttrs ["className" =: toJSString c]

-- | Render a string of text.
text :: String -> Slide
text s = lift $ newElem "div" `with` ["textContent"   =: toJSString s,
                                      style "margin"  =: "0px",
                                      style "padding" =: textPadding]

-- | Render a block of code with a fixed-width font.
--   The code is escaped according to the rules of 'Markup'.
code :: String -> Slide
code s = lift $ do
  inner <- newElem "pre" `with` [
               "innerHTML"        =: toJSString s,
               style "text-align" =: "left",
               style "margin"     =: "0px",
               style "padding"    =: textPadding
             ]
  newElem "div" `with` [children [inner], style "display" =: "inline-block"]

-- | Render a string of text possibly containing markup.
--   When using @OverloadedStrings@ the string literal @"hello"@ is equivalent
--   to @markup "hello"@.
markup :: String -> Slide
markup s = lift $ newElem "div" `with` ["innerHTML"     =: render (fromString s),
                                        style "margin"  =: "0px",
                                        style "padding" =: textPadding]

-- | Render an image.
image :: URL -> Slide
image url = lift $ newElem "img" `with` ["src" =: toJSString url]

data List = Sublist ListStyle Markup [List] | Line Markup

instance IsString List where
  fromString = Line . fromString

-- | Create a list of items. List items may be either text strings or sublists.
list :: ListStyle -> [List] -> Slide
list listtype rows = lift $ do
    e <- newElem (listStyleString listtype) `with` [
           style "margin" =: "0px",
           style "padding" =: textPadding,
           style "list-style-position" =: "inside",
           style "text-align" =: "left",
           style "display" =: "inline-block"]
    mapM_ (mkListItem >=> appendChild e) rows
    return e
  where
    mkListItem (Line s) =
      newElem "li" `with` ["innerHTML" =: render s,
                           style "text-align" =: "left"]
    mkListItem (Sublist sty heading xs) = do
      e <- newElem (listStyleString sty)
      mapM_ (mkListItem >=> appendChild e) xs
      newElem "li" `with` ["innerHTML" =: render heading,
                           children [e]]

-- | Create a sublist from a list style, a sublist heading, and a list of
--   list items.
sublist :: ListStyle -> Markup -> [List] -> List
sublist = Sublist

-- | Render the given slide using the given color.
color :: String -> Slide -> Slide
color c = withAttrs [style "color" =: toJSString c]

-- | Render the text and other primitive elements in the given slide with the
--   given background color.
textBackground :: String -> Slide -> Slide
textBackground c = withAttrs [style "background-color" =: toJSString c]

-- | Render the given slide using the given background color for its whole
--   layout group.
backgroundColor :: String -> Slide -> Slide
backgroundColor c = groupAttrs [style "background-color" =: toJSString c]

-- | Display the given slide with the given font size.
fontSize :: Size -> Slide -> Slide
fontSize sz = withAttrs [style "fontSize" =: sizeString sz]

-- | Use the given amount of padding around the given slide.
padded :: Size -> Slide -> Slide
padded sz = withAttrs [style "padding" =: sizeString sz]

-- | Display the given slide with the given font face.
font :: String -> Slide -> Slide
font f = withAttrs [style "font-family" =: toJSString f]

-- | Display the given slide with the given alignment.
aligned :: Alignment -> Slide -> Slide
aligned al = groupAttrs [style "text-align" =: alignString al]

-- | Display the contents of the given slide centered.
centered :: Slide -> Slide
centered = aligned Center

-- | Display the contents of the given slide vertically centered.
verticallyCentered :: Slide -> Slide
verticallyCentered s = groupAttrs [style "position" =: "absolute",
                                   style "top" =: "50%",
                                   style "margin-top" =: hh s] s
  where
    hh slide = J.concat ["-", toJSString $ getHeight [] slide/2, "px"]

    -- compute the height of a slide's contents by inserting it into the
    -- document with the appropriate parameters and measuring its height
    height :: [Attribute] -> Slide -> Double
    height as slide = unsafePerformIO $ do
      e <- toElem slide
      c <- newElem "div" `with` (children[e] : as)
      appendChild documentBody c
      h <- getProp e "clientHeight"
      deleteChild documentBody c
      return $ maybe 0 id $ fromJSString h

    getHeight as (PStyle as' slide) = getHeight (as ++ as') slide
    getHeight as (Row ss)           = maximum (map (getHeight as) ss)
    getHeight as c@(Col _)          = heightOfCol as c
    getHeight as slide              = height as slide

    -- get the height of a column by inserting it into the document with a
    -- "sentinel" element appended at the end of the last element in the column
    -- and inspecting the offset of that sentinel
    heightOfCol attrs c = unsafePerformIO $ do
      e <- toElem c
      mlast <- getLastChild e
      flip (maybe (return 0)) mlast $ \laste -> do
        cont <- newElem "div" `with` (children [e] : attrs)
        appendChild documentBody cont
        off <- getElemOffsetPlusHeight laste e
        deleteChild documentBody cont
        return off

getElemOffsetPlusHeight :: Elem -> Elem -> IO Double
getElemOffsetPlusHeight = ffi "function(e,container) {\
var r = e.getBoundingClientRect();\
var pr = container.getBoundingClientRect();\
return e.firstChild.clientHeight + (r.top - pr.top);}"

-- | Put the first slide above the second.
above :: Slide -> Slide -> Slide
above (Col as) (Col bs) = Col (as ++ bs)
above (Col as) b        = Col (as ++ [b])
above a        (Col bs) = Col (a : bs)
above a        b        = Col [a, b]

infixl 3 `above`

-- | Put the first slide to the left of the second.
leftOf :: Slide -> Slide -> Slide
leftOf (Row ls) (Row rs) = Row (ls ++ rs)
leftOf (Row ls) r        = Row (ls ++ [r])
leftOf l        (Row rs) = Row (l : rs)
leftOf l        r        = Row [l, r]

infixl 4 `leftOf`

-- | Request that the given fraction of the available space is allocated to
--   the given element. For instance, @sized 0.25 s@ will request that @s@ is
--   allocated 25% of the available screen space, while any elements it is
--   composed with get to share the remaining 75%.
sized :: Double -> Slide -> Slide
sized = SizeReq

infixl 5 `sized`

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
--
--   For the purposes of styling, when a list of attributes is applied to a
--   group using 'withAttrs', the attributes will be applied to the contents of
--   the layout group and not the group itself. In order to apply styling to
--   the layout group itself, use 'groupAttrs' instead.
group :: Slide -> Slide
group = PStyle []

-- | Create a row of slides, with all slides taking up equal space.
row :: [Slide] -> Slide
row = foldl1' leftOf

-- | Create a column of slides, with all slides taking up equal space.
column :: [Slide] -> Slide
column = foldl1' above
