module Blessed where

import Prelude as Prelude
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Options (Options, optional, options, (:=), opt, Option)
import Prelude hiding (show,append,bottom)

foreign import data BLESS :: !
foreign import data Node :: * -> *
foreign import data Screen :: *
foreign import data Element :: * -> *
foreign import data Form :: *
foreign import data List :: * -> *
foreign import data Textbox :: *
foreign import data Text :: *

type ScreenOptions =
  { smartCSR :: Boolean
  , title :: String
  }

defaultScreenOptions :: ScreenOptions
defaultScreenOptions = {smartCSR: true, title: "Blessed"}

newtype Distance = Distance Foreign

colDistance :: Int -> Distance
colDistance i = Distance (toForeign i)

percentDistance :: Int -> Distance
percentDistance i = Distance (toForeign (Prelude.show i <> "%"))

type NodeOptions a = Options (Node a)

screen' :: forall a. Option (Node a) (Maybe Screen)
screen' = optional (opt "screen")

parent :: forall a b. Option (Node a) (Maybe (Node b))
parent = optional (opt "parent")

children :: forall a b. Option (Node a) (Maybe (Array (Node b)))
children = optional (opt "children")

defaultNodeOptions :: forall a. NodeOptions a
defaultNodeOptions =
  screen' := Nothing
  <> parent := Nothing
  <> children := Nothing

-- Element Options

type ElementOptions a = Options (Element a)

top :: forall a. Option (Element a) (Maybe Distance)
top = optional (opt "top")

bottom :: forall a. Option (Element a) (Maybe Distance)
bottom = optional (opt "bottom")

left ::  forall a. Option (Element a) (Maybe Distance)
left = optional (opt "left")

width :: forall a. Option (Element a) (Maybe Distance)
width = optional (opt "width")

height :: forall a. Option (Element a) (Maybe Distance)
height = optional (opt "height")

content :: forall a. Option (Element a) (Maybe String)
content = optional (opt "content")

style :: forall a. Option (Element a) (Maybe {fg :: String, bg :: String})
style = optional (opt "style")

defaultElementOptions :: forall a. ElementOptions a
defaultElementOptions =
  bottom     := Nothing
  <> left    := Nothing
  <> width   := Just (percentDistance 100)
  <> height  := Nothing
  <> content := Nothing
  <> style   := Just { fg: "white" , bg: "black"}

-- Text Options
type TextOptions = Options (Element Text)

align :: Option (Element Text) (Maybe String)
align = optional (opt "align")

defaultTextOptions :: TextOptions
defaultTextOptions = defaultElementOptions <> align := Nothing

-- Textbox Options
type TextboxOptions = Options (Element Textbox)

tags :: Option (Element Textbox) (Maybe Boolean)
tags = optional (opt "tags")

defaultTextboxOptions :: TextboxOptions
defaultTextboxOptions = defaultElementOptions <> tags := Just true

-- Form Options
type FormOptions = Options (Element Form)

label :: Option (Element Form) (Maybe String)
label = optional (opt "label")

defaultFormOptions :: FormOptions
defaultFormOptions = defaultElementOptions <> label := Nothing

-- List Options
type ListOptions a = Options (Element (List a))

listMouse :: forall a. Option (Element (List a)) (Maybe Boolean)
listMouse = optional (opt "mouse")

listKeys :: forall a. Option (Element (List a)) (Maybe Boolean)
listKeys = optional (opt "keys")

listVi :: forall a. Option (Element (List a)) (Maybe Boolean)
listVi = optional (opt "vi")

items :: forall a. Option (Element (List a)) (Maybe (Array String))
items = optional (opt "items")

interactive :: forall a. Option (Element (List a)) (Maybe Boolean)
interactive = optional (opt "keys")

defaultListOptions :: forall a. ListOptions a
defaultListOptions = defaultElementOptions
  <> listMouse   := Nothing
  <> listKeys    := Nothing
  <> listVi      := Just true
  <> items       := Just ["hallo", "welt", "bob"]
  <> interactive := Nothing

foreign import screen
  :: forall e
  . ScreenOptions
  -> Eff (bless :: BLESS | e) (Element Screen)

form :: forall e. FormOptions -> Eff (bless :: BLESS | e) (Element Form)
form o = formImpl (options o)

foreign import formImpl
  :: forall e
  . Foreign
  -> Eff (bless :: BLESS | e) (Element Form)

textbox :: forall e. TextboxOptions -> Eff (bless :: BLESS | e) (Element Textbox)
textbox o = textboxImpl (options o)

foreign import textboxImpl
  :: forall e
  . Foreign
  -> Eff (bless :: BLESS | e) (Element Textbox)

text :: forall e . TextOptions -> Eff (bless :: BLESS | e) (Element Text)
text o = textImpl (options o)

foreign import textImpl
  :: forall e
  . Foreign
  -> Eff (bless :: BLESS | e) (Element Text)

list
  :: forall a e . ListOptions a -> Eff (bless :: BLESS | e) (Element (List Unit))
list o = listImpl (options o)

foreign import listImpl
  :: forall e
  . Foreign
  -> Eff (bless :: BLESS | e) (Element (List Unit))

foreign import clearItems
  :: forall a e
  . Element (List a)
  -> Eff (bless :: BLESS | e) Unit

foreign import setItems
  :: forall a e
  . Element (List a)
  -> Array String
  -> Eff (bless :: BLESS | e) Unit

foreign import render
  :: forall e
  . Element Screen
  -> Eff (bless :: BLESS | e) Unit

foreign import append
  :: forall a1 a2 e
  . Element a1
  -> Element a2
  -> Eff (bless :: BLESS | e) Unit

foreign import remove
  :: forall a1 a2 e
  . Element a1
  -> Element a2
  -> Eff (bless :: BLESS | e) Unit

foreign import clearChildren
  :: forall a1 e
  . Element a1
  -> Eff (bless :: BLESS | e) Unit

foreign import key
  :: forall a e
  . Element a
  -> String
  -> Eff (bless :: BLESS | e) Unit
  -> Eff (bless :: BLESS | e) Unit

foreign import onSelect
  :: forall a e
  . Element (List a)
  -> (Int -> Eff (bless :: BLESS | e) Unit)
  -> Eff (bless :: BLESS | e) Unit

foreign import hide
  :: forall a e
  . Element a
  -> Eff (bless :: BLESS | e) Unit

foreign import show
  :: forall a e
  . Element a
  -> Eff (bless :: BLESS | e) Unit

foreign import focus
  :: forall a e
  . Element a
  -> Eff (bless :: BLESS | e) Unit

foreign import readInput
  :: forall e
  . Element Textbox
  -> (String -> Eff (bless :: BLESS | e) Unit)
  -> Eff (bless :: BLESS | e) Unit

foreign import clearValue
  :: forall e
  . Element Textbox
  -> Eff (bless :: BLESS | e) Unit

foreign import setValue
  :: forall e
  . Element Textbox
  -> String
  -> Eff (bless :: BLESS | e) Unit
