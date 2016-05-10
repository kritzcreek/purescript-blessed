module Blessed where

import Control.Monad.Eff (Eff)
import Data.Foreign (toForeign, Foreign)
import Prelude
import Prelude as P

foreign import data BLESS :: !
foreign import data Screen :: *
foreign import data Element :: * -> *
foreign import data Form :: * -> *
foreign import data Textbox :: * -> *

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

type ElementO e =
  ( bottom :: Distance
  , left   :: Distance
  , width  :: Distance
  , height :: Distance
  , style  :: { fg :: String
              , bg :: String
              }
  | e
  )

type TextboxO e =
 ( tags :: Boolean
 | e
 )

type TextboxOptions = Object (ElementO (TextboxO ()))

defaultFormOptions :: FormOptions
defaultFormOptions =
  { bottom: colDistance 0
  , left:   colDistance 0
  , width:  percentDistance 100
  , height: colDistance 10
  , style: { fg: "black"
           , bg: "white"
           }
  , label: "Form"
  }

defaultTextboxOptions :: TextboxOptions
defaultTextboxOptions =
  { width: percentDistance 100
  , bottom: percentDistance 100
  , height: colDistance 1
  , left: colDistance 0
  , tags: true
  , style: { fg: "white"
           , bg: "black"
           }
  }

type FormO e =
  ( label :: String
  | e
  )

type FormOptions = Object (ElementO (FormO ()))

foreign import screen
  :: forall e
  . ScreenOptions
  -> Eff (bless :: BLESS | e) (Element Screen)

foreign import form
  :: forall e
  . FormOptions
  -> Eff (bless :: BLESS | e) (Element (Form Unit))

foreign import textbox
  :: forall e
  . TextboxOptions
  -> Eff (bless :: BLESS | e) (Element (Textbox Unit))

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
  :: forall a e
  . (Element (Textbox a))
  -> Eff (bless :: BLESS | e) Unit

-- TODO: Interface "Input" implementieren
foreign import readInput
  :: forall a e
  . (Element (Textbox a))
  -> (String -> Eff (bless :: BLESS | e) Unit)
  -> Eff (bless :: BLESS | e) Unit

foreign import clearValue
  :: forall a e
  . (Element (Textbox a))
  -> Eff (bless :: BLESS | e) Unit

foreign import setValue
  :: forall a e
  . (Element (Textbox a))
  -> String
  -> Eff (bless :: BLESS | e) Unit
