{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Haste.Deck where
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

withAttrs :: [Attribute] -> Slide a -> Slide a
withAttrs attrs s = do
  x <- s
  get >>= flip set attrs
  return x

getDimensions :: Elem -> IO (Int, Int)
getDimensions = ffi "(function(e){return [e.offsetWidth, e.offsetHeight];})"

-- | Render a string of text.
text :: String -> Slide ()
text s = liftIO (newElem "span" `with` ["textContent" =: s]) >>= put

-- | Render the given slide using the given color.
color :: String -> Slide a -> Slide a
color c = withAttrs [style "color" =: c]

-- | Display the given slide with the given font size.
fontSize :: FontSize -> Slide a -> Slide a
fontSize sz = withAttrs [style "fontSize" =: show sz]

-- | Display the given slide with the given font face.
font :: String -> Slide a -> Slide a
font f = withAttrs [style "font-family" =: f]

-- | Display the given slide with the given CSS class.
withClass :: String -> Slide a -> Slide a
withClass c = withAttrs ["class" =: c]

-- | Display the given slide centered.
centered :: Slide a -> Slide a
centered s = do
  x <- s
  e <- get
  e' <- newElem "div" `with` [children [e],
                              style "text-align" =: "center",
                              style "display" =: "inline-block"]
  put e'
  return x

-- | Display the given slide centered.
verticallyCentered :: Slide a -> Slide a
verticallyCentered s = do
  x <- s
  e <- get
  set e [style "position" =: "absolute",
         style "top" =: "50%",
         style "transform" =: "translate(0,-50%)"]
  e' <- newElem "div" `with` [children [e],
                              style "position" =: "absolute",
                              style "border" =: "1px solid black",
                              style "height" =: "100%",
                              style "width" =: "100%",
                              style "display" =: "inline-block"]
  put e'
  return x

-- | Put the first slide above the second.
above :: Slide () -> Slide () -> Slide ()
above a b = do
  a' <- a >> get
  b' <- b >> get
  br <- newElem "br"
  put =<< newElem "div" `with` [children [a',br,b'],
                                style "display" =: "inline-block"]

-- | Put the first slide to the left of the second.
leftOf :: Slide () -> Slide () -> Slide ()
leftOf l r = do
  l' <- l >> get
  r' <- r >> get
  put =<< newElem "div" `with` [children [l',r'],
                                style "display" =: "inline-block"]
row :: [Slide ()] -> Slide ()
row = foldl1' (flip leftOf)

column :: [Slide ()] -> Slide ()
column = foldl1' (flip above)

-- | Render an image.
image :: URL -> Slide ()
image url = (put =<<) . liftIO $ newElem "img" `with` ["src" =: url]
