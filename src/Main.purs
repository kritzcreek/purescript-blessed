module Main where

import Blessed
import Node.Process as P
import Control.Monad.Eff (Eff)
import Data.Array (length, (!!))
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Options ((:=))
import Prelude ((<>), bind, Unit)

main :: forall e. Eff ( bless :: BLESS, process :: P.PROCESS | e) Unit
main = do

  s <- screen defaultScreenOptions
  f <- form (defaultFormOptions
             <> label  := Just "Pursuit"
             <> bottom := Just (colDistance 0)
             <> width  := Just (percentDistance 100)
             <> height := Just (colDistance 2))

  listForm <- form (defaultFormOptions
                   <> label  := Just "Pursuit Results"
                   <> top := Just (colDistance 2)
                   <> width  := Just (percentDistance 100))
  input <- textbox (defaultTextboxOptions
                    <> bottom := Just (colDistance 0)
                    <> left   := Just (colDistance 2)
                    <> height := Just (colDistance 1))

  label <- text (defaultTextOptions
                     <> content := Just "PURR"
                     <> top     := Just (colDistance 0)
                     <> height  := Just (colDistance 1))

  d <- textbox (defaultTextboxOptions
                    <> bottom := Just (colDistance 2)
                    <> height := Just (colDistance 1)
                    <> style  := Just {fg: "red", bg: "black"})
  append f input
  append s f
  append s listForm
  append s d
  append s label

  key s "q" (P.exit 0)
  key s "p" do
    show f
    clearValue input
    render s
    readInput input (\i -> do
                        setValue d i
                        hide f
                        render s)
  key s "l" do
    let values = ["Waow", "Rofl", "Copter"]
    show listForm
    displayList listForm values
      \i -> do
        setValue d (fromMaybe "" (values !! i))
        hide listForm
        render s
    render s
  hide f
  hide listForm
  render s

displayList
  :: forall a e
  . Element a
  -> Array String
  -> (Int -> Eff (bless :: BLESS | e) Unit)
  -> Eff (bless :: BLESS | e) Unit
displayList scr choices cb = do
  lst <- list (defaultListOptions
               <> top         := Just (colDistance 4)
               <> height      := Just (colDistance (length choices))
               <> interactive := Just true
               <> items       := Just choices
               <> style       := Just {fg: "blue", bg: "black"})
  append scr lst
  focus lst
  onSelect lst cb
