{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module WebMVC.Types
  ( NameSpace(..)
  , Tag
  , AttributeName
  , StyleAttribute, StyleValue
  , EventName
  , AttributeValue(..)
  , Attributes
  , Attribute
  , Tree(..)
  , View

  , AttrDelta(..)
  ) where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import qualified Lucid.Base as Lucid
import           WebMVC.Diff (Delta(..), Diff(..), Changed(Changed))
import qualified WebMVC.Diff as Diff

--------------------------------------------------------------------------------

data NameSpace = Html | Svg
  deriving (Show,Read,Eq,Ord)

instance Diff.HasDiff NameSpace

type Tag = Text

type AttributeName = Text

type StyleAttribute = Text
type StyleValue     = Text

type EventName = Text

data AttributeValue action = Property Text
                           | Event EventName action
                           | Style (Map.Map StyleAttribute StyleValue)
                           deriving (Show,Eq,Functor,Foldable,Traversable)


data AttrDelta action =
    PropChanged (Delta Text)
  | EventChanged (These (Delta EventName) (Delta action))
  | StyleChanged (Delta (Map.Map StyleAttribute StyleValue))
  | OtherType (Diff.Changed (AttributeValue action))

deriving instance (Show action, Show (Delta action)) => Show (AttrDelta action)

instance Diff.HasDiff action => Diff.HasDiff (AttributeValue action) where
  type Delta (AttributeValue action) = AttrDelta action

  diff (Property old)         (Property new)         = PropChanged <$> diff old new
  diff (Event oldName oldAct) (Event newName newAct) =
    case (diff oldName newName, diff oldAct newAct) of
      (NoDifference, NoDifference) -> NoDifference
      (Difference l, NoDifference) -> Difference $ EventChanged (This l)
      (NoDifference, Difference r) -> Difference $ EventChanged (That r)
      (Difference l, Difference r) -> Difference $ EventChanged (These l r)

  diff (Style old)            (Style new)            = StyleChanged <$> diff old new
  diff old                    new                    = Difference $ OtherType (Changed old new)


type Attributes action = Map.Map AttributeName (AttributeValue action)


toLucidAttributes  :: Attributes action -> [Lucid.Attribute]
toLucidAttributes = Map.foldMapWithKey mkAttr
  where
    mkAttr k = \case
        Property v    -> case propertyValue k v of
                           Nothing -> []
                           Just v' -> [ Lucid.makeAttribute k v' ]
        Event _ _     -> []
        Style styleM  -> let v           = Map.foldrWithKey go mempty styleM
                             go k' v' ys = mconcat [ k', ":", v', ";" ] <> ys
                         in [ Lucid.makeAttribute k v ]

    propertyValue k v
      | isException && v == "True"        = Just k
      | not (isException && v == "False") = Just v
      | otherwise                         = Nothing
      where
        isException = k `elem` exceptions

    exceptions = [ "checked"
                 , "disabled"
                 , "selected"
                 , "hidden"
                 , "readOnly"
                 , "autoplay"
                 , "required"
                 , "default"
                 , "autofocus"
                 , "multiple"
                 , "noValidate"
                 , "autocomplete"
                 ]

--------------------------------------------------------------------------------

data Tree action = TextNode Text.Text
                 | Element {-# UNPACK#-}!NameSpace
                           Tag
                           (Attributes action)
                           [Tree action]
                 deriving stock (Show,Eq,Functor,Foldable,Traversable)

instance Lucid.ToHtml (Tree action) where
  toHtmlRaw = Lucid.toHtml
  toHtml (TextNode t)          = Lucid.toHtml t
  toHtml (Element _ t ats chs) = Lucid.with ele (toLucidAttributes ats)
    where
      tag = Text.toLower t
      ele | tag `elem` noEnd = Lucid.makeElementNoEnd tag
          | otherwise        = Lucid.makeElement tag kids

      noEnd = ["img", "input", "br", "hr", "meta"]

      kids = foldMap Lucid.toHtml $ collapseSiblingTextNodes chs

      collapseSiblingTextNodes = \case
        []                             -> []
        (TextNode x : TextNode y : xs) -> collapseSiblingTextNodes (TextNode (x <> y) : xs)
        (x:xs)                         -> x : collapseSiblingTextNodes xs


data TreeDelta action =
    TextNodeChanged (Delta Text.Text)
  | ElementChanged (These (Delta (Attributes action)) (Delta [Tree action]))
  | TreeTypeChanged (Changed (Tree action))

deriving instance ( Show action
                  , Show (Delta action)
                  , Show (Delta [Tree action])
                  ) => Show (TreeDelta action)

-- instance Diff.HasDiff action => Diff.HasDiff (Tree action) where
--   type Delta (Tree action) = TreeDelta action

--   diff (TextNode old)
--        (TextNode new) = TextNodeChanged <$> diff old new
--   diff (Element oldNs oldTag oldAts oldChs)
--        (Element newNs newTag newAts newChs)
--     | oldNs == newNs && oldTag == newTag = case (diff oldAts newAts, diff oldChs newChs) of
--         (NoDifference, NoDifference) -> NoDifference
--         (Difference l, NoDifference) -> Difference $ ElementChanged (This l)
--         (NoDifference, Difference r) -> Difference $ ElementChanged (That r)
--         (Difference l, Difference r) -> Difference $ ElementChanged (These l r)

--   diff old new = Difference $ TreeTypeChanged (Changed old new)
--     -- too many chagnes

--------------------------------------------------------------------------------







--------------------------------------------------------------------------------

type View action = Tree action

type Attribute action = Map.Map Text (AttributeValue action)
