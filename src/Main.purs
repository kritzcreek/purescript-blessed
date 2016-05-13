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

  title <- text (defaultTextOptions
                     <> content := Just "PURR"
                     <> top     := Just (colDistance 0)
                     <> height  := Just (colDistance 1))
  append s title

  pursuitInputF <- form (defaultFormOptions
                         <> label  := Just "Pursuit"
                         <> bottom := Just (colDistance 0)
                         <> width  := Just (percentDistance 100)
                         <> height := Just (colDistance 2))

  pursuitInput <- textbox (defaultTextboxOptions
                    <> bottom := Just (colDistance 0)
                    <> left   := Just (colDistance 2)
                    <> height := Just (colDistance 1))

  append pursuitInputF pursuitInput
  append s pursuitInputF
  hide pursuitInputF

  pursuitResult <- form (defaultFormOptions
                      <> label  := Just "Pursuit Results"
                      <> top := Just (colDistance 2)
                      <> width  := Just (percentDistance 100))
  psList <- pursuitList


  -- d <- textbox (defaultTextboxOptions
  --                   <> bottom := Just (colDistance 2)
  --                   <> height := Just (colDistance 1)
  --                   <> style  := Just {fg: "red", bg: "black"})

  key s "q" (P.exit 0)
  key s "p" do
    show pursuitInputF
    clearValue pursuitInput
    render s
    readInput pursuitInput (\i -> do
                               hide pursuitInputF
                               render s)
  -- key s "l" do
  --   let values = ["Waow", "Rofl", "Copter"]
  --   displayList listForm values
  --     \i -> do
  --       setValue d (fromMaybe "" (values !! i))
  --       hide listForm
  --       render s
  --   render s
  render s
pursuitList :: forall e. Eff (bless :: BLESS | e) (Element (List Unit))
pursuitList =
  list (defaultListOptions
        <> top         := Just (colDistance 4)
        <> height      := Just (percentDistance 100)
        <> interactive := Just true
        <> style       := Just {fg: "blue", bg: "black"})
