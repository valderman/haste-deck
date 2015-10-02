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
--       The title attribute is currently not supported.
newtype Markup = Markup {unM :: JSString}

instance IsString Markup where
  fromString = Markup . fromString

-- | Render the given 'Markup' into a 'JSString' of HTML.
render :: Markup -> JSString
render = em . strong . code . link . unM

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

link :: JSString -> JSString
link s = replace s (regex "\\[(.*?)\\]\\((.*?)\\)" "g") "<a href=\"$2\">$1</a>"
