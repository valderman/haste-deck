{-# LANGUAGE OverloadedStrings #-}
module Haste.Deck.Markup (Markup, render) where
import Data.String
import Haste (JSString)
import Haste.JSString as S

-- | A string possibly containing markup.
--   The markup used by Haste.Deck is a subset of Markdown, containing:
--
--     * Single and double asterisks, for EM and STRONG tags respectively.
--     * Backticks for inline code.
--     * Angle brackets and parenthesis for links.
--       (The title attribute is currently not supported.)
--     * Newlines turn into @<br>@ tags.
newtype Markup = Markup {unM :: JSString}

instance IsString Markup where
  fromString = Markup . fromString

-- | Render the given 'Markup' into a 'JSString' of HTML.
render :: Markup -> JSString
render = link (newline . em . strong . code) . unM

replaceTag :: JSString -> JSString -> JSString -> JSString
replaceTag markup tag s =
  replace s (regex (S.concat [markup, "(.*?)", markup]) "g")
            (S.concat ["<", tag, ">$1</", tag, ">"])

strong :: JSString -> JSString
strong = replaceTag "\\*\\*" "strong" . replaceTag "__" "strong"

em :: JSString -> JSString
em = replaceTag "\\*" "em" . replaceTag "_" "em"

code :: JSString -> JSString
code = replaceTag "`" "code"

-- | Create markup for link, then apply additional markdown to the non-URL
--   part.
link :: (JSString -> JSString) -> JSString -> JSString
link f s = Prelude.foldr (.) id links (f $ replace s re "[L](U)")
  where
    re = regex "\\[(.*?)\\]\\((.*?)\\)" "g"
    rawlinks = match re s
    labels = Prelude.map (\s -> f $ replace s re "$1") rawlinks
    urls = Prelude.map (\s -> replace s re "$2") rawlinks
    links = zipWith (\l u s -> replace s re (mklink l u)) labels urls
    mklink l u = S.concat ["<a href=\"", u, "\">", l, "</a>"]

newline :: JSString -> JSString
newline s = replace s (regex "(\n)" "g") "<br>"
