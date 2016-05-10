module Main where

import Blessed
import Control.Monad.Eff (Eff)
import Node.Process as P
import Prelude (bind, Unit)

main :: forall e. Eff ( bless :: BLESS, process :: P.PROCESS | e) Unit
main = do

  s <- screen defaultScreenOptions
  f <- form (defaultFormOptions { label  = "PSCID"
                                , width  = percentDistance 100
                                , height = colDistance 2
                                })
  t <- textbox (defaultTextboxOptions { bottom = colDistance 0
                                      })
  d <- textbox (defaultTextboxOptions { bottom = colDistance 3
                                      , style = {fg: "red", bg: "black"}
                                      })
  append f t
  append s f
  append s d
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
