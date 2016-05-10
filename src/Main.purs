module Main where

import Blessed
import Node.Process as P
import Control.Monad.Eff (Eff)
import Data.Options ((:=))
import Prelude ((<>), bind, Unit)

main :: forall e. Eff ( bless :: BLESS, process :: P.PROCESS | e) Unit
main = do

  s <- screen defaultScreenOptions
  f <- formImpl (defaultFormOptions
                 <> label  := "Pursuit"
                 <> width  := percentDistance 100
                 <> height := colDistance 2)
  t <- textboxImpl (defaultTextboxOptions
                    <> bottom := colDistance 0
                    <> left   := colDistance 2)
  label <- textImpl (defaultTextOptions
                     <> content := "PSCID"
                     <> bottom := colDistance 3)

  d <- textboxImpl (defaultTextboxOptions
                    <> bottom := colDistance 2
                    <> style := {fg: "red", bg: "black"})
  append f t
  append s f
  append s d
  append s label
  key s "q" (P.exit 0)
  key s "p" do
    show f
    clearValue t
    render s
    readInput t (\i -> do
                    setValue d i
                    hide f
                    render s)
  hide f
  render s
