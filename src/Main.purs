module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff (awaitBody, runHalogenAff, HalogenEffects)
import Halogen.VDom.Driver (runUI)
import Container as Container

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI Container.component unit body
