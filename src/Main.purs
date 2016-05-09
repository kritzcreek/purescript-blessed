module Main where

import Prelude
import Blessed (appendForm, render, BLESS, defaultScreenOptions, defaultFormOptions, form, screen)
import Control.Monad.Eff (Eff)

main :: forall e. Eff ( bless :: BLESS | e) Unit
main = do
  s <- screen defaultScreenOptions
  f <- form (defaultFormOptions {label="PSCID", width=})
  appendForm s f
  render s

