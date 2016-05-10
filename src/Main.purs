module Main where

import Blessed
import Node.Process as P
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(Just))
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
  input <- textbox (defaultTextboxOptions
                    <> bottom := Just (colDistance 0)
                    <> left   := Just (colDistance 2)
                    <> height := Just (colDistance 1))

  label <- text (defaultTextOptions
                     <> content := Just "PSCID"
                     <> bottom  := Just (colDistance 3)
                     <> height  := Just (colDistance 1))

  d <- textbox (defaultTextboxOptions
                    <> bottom := Just (colDistance 2)
                    <> height := Just (colDistance 1)
                    <> style  := Just {fg: "red", bg: "black"})
  myList <- list (defaultListOptions
                    <> bottom      := Just (colDistance 4)
                    <> height      := Just (colDistance 5)
                    <> interactive := Just true
                    <> style       := Just {fg: "blue", bg: "greeen"})
  append f input
  append s f
  append s d
  append s label
  append s myList
  key s "q" (P.exit 0)
  key s "p" do
    show f
    clearValue input
    render s
    readInput input (\i -> do
                        setValue d i
                        hide f
                        render s)
  hide f
  focus myList
  render s
