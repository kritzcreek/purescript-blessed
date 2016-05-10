module Blessed where

import Prelude
import Prelude as P
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign, toForeign)
import Data.Options (options, (:=), opt, Option, Options)
import Unsafe.Coerce (unsafeCoerce)

foreign import data BLESS :: !
foreign import data Screen :: *
foreign import data Element :: * -> *
foreign import data Form :: *
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
percentDistance i = Distance (toForeign (P.show i <> "%"))

-- Element Options

type ElementOptions a = Options (Element a)

bottom :: forall a. Option (Element a) Distance
bottom = opt "bottom"

left ::  forall a. Option (Element a) Distance
left = opt "left"

width :: forall a. Option (Element a) Distance
width = opt "width"

height :: forall a. Option (Element a) Distance
height = opt "height"

content :: forall a. Option (Element a) String
content = opt "content"

style :: forall a. Option (Element a) {fg :: String, bg :: String}
style = opt "style"

defaultElementOptions :: forall a. ElementOptions a
defaultElementOptions =
  bottom     := colDistance 0
  <> left    := colDistance 0
  <> width   := percentDistance 100
  <> height  := colDistance 1
  <> content := ""
  <> style   := { fg: "white" , bg: "black"}

-- Text Options
type TextOptions = Options (Element Text)

align :: Option (Element Text) String
align = opt "align"

defaultTextOptions :: TextOptions
defaultTextOptions = defaultElementOptions <> align := "left"

-- Textbox Options
type TextboxOptions = Options (Element Textbox)

tags :: Option (Element Textbox) Boolean
tags = opt "tags"

defaultTextboxOptions :: TextboxOptions
defaultTextboxOptions = defaultElementOptions <> tags := true

-- Form Options
type FormOptions = Options (Element Form)

label :: Option (Element Form) String
label = opt "label"

defaultFormOptions :: FormOptions
defaultFormOptions = defaultElementOptions <> label := ""

foreign import screen
  :: forall e
  . ScreenOptions
  -> Eff (bless :: BLESS | e) (Element Screen)

formImpl :: forall e. FormOptions -> Eff (bless :: BLESS | e) (Element Form)
formImpl o = form (options o)

foreign import form
  :: forall e
  . Foreign
  -> Eff (bless :: BLESS | e) (Element Form)

textboxImpl :: forall e. TextboxOptions -> Eff (bless :: BLESS | e) (Element Textbox)
textboxImpl o = textbox (options o)

foreign import textbox
  :: forall e
  . Foreign
  -> Eff (bless :: BLESS | e) (Element Textbox)

textImpl :: forall e . TextOptions -> Eff (bless :: BLESS | e) (Element Text)
textImpl o = text (options o)

foreign import text
  :: forall e
  . Foreign
  -> Eff (bless :: BLESS | e) (Element Text)

foreign import render
  :: forall e
  . Element Screen
  -> Eff (bless :: BLESS | e) Unit

foreign import append
  :: forall a1 a2 e
  . Element a1
  -> Element a2
  -> Eff (bless :: BLESS | e) Unit

foreign import key
  :: forall a e
  . Element a
  -> String
  -> Eff (bless :: BLESS | e) Unit
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
  :: forall e
  . Element Textbox
  -> Eff (bless :: BLESS | e) Unit

-- TODO: Interface "Input" implementieren
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
