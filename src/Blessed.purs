module Blessed where

import Control.Monad.Eff (Eff)
import Prelude (Unit)

foreign import data BLESS :: !
foreign import data Screen :: *
foreign import data Form :: *

type ScreenOptions =
  { smartCSR :: Boolean
  , title :: String
  }

defaultScreenOptions :: ScreenOptions
defaultScreenOptions = {smartCSR: true, title: "Blessed"}

type Distance = Int

type ElementO e =
  ( bottom :: Distance
  , left   :: Distance
  , width  :: Distance
  , height :: Distance
  , style :: {fg :: String, bg :: String}
  | e
  )

defaultFormOptions :: FormOptions
defaultFormOptions =
  { bottom: 0
  , left:   0
  , width:  10
  , height: 10
  , style: {fg: "black", bg: "white"}
  , label: "Form"
  }

type FormO e =
  ( label :: String
  | e
  )

type FormOptions = Object (ElementO (FormO ()))

foreign import screen
  :: forall e
  . ScreenOptions
  -> Eff (bless :: BLESS | e) Screen

foreign import form
  :: forall e
  . FormOptions
  -> Eff (bless :: BLESS | e) Form

foreign import render
  :: forall e
  . Screen
  -> Eff (bless :: BLESS | e) Unit

foreign import appendForm
  :: forall e
  . Screen
  -> Form
  -> Eff (bless :: BLESS | e) Unit

