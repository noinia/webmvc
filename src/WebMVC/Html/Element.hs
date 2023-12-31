{-# LANGUAGE OverloadedStrings #-}
module WebMVC.Html.Element
  ( html_
  , head_
  , body_
  -- * Primitives
  , elementHtml
  , property_
  , style_
  -- * Tags for in the head
  , title_
  -- * Body
  , div_
  , text_
  ) where

import qualified Data.Map as Map
import           Data.Text (Text)
import           WebMVC.Types
import           WebMVC.Util

--------------------------------------------------------------------------------

elementHtml         :: Tag -> [Attribute action] -> [View action] -> View action
elementHtml tag ats = Element Html tag (mconcat ats)

html_ :: [Attribute action] -> [View action] -> View action
html_ = elementHtml "html"

head_ :: [Attribute action] -> [View action] -> View action
head_ = elementHtml "head"

body_ :: [Attribute action] -> [View action] -> View action
body_ = elementHtml "body"

div_ :: [Attribute action] -> [View action] -> View action
div_ = elementHtml "div"

text_ :: Text -> View action
text_ = TextNode

property_     :: Text -> Text -> Attribute action
property_ k v = k =: Property v

title_ :: Text -> Attribute action
title_ = property_ "title"

style_     :: [Map.Map StyleAttribute StyleValue] -> Attribute action
style_ ats = "style" =: Style (mconcat ats)
