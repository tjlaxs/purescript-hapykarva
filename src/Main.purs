module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Container as Container

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI Container.component unit body
