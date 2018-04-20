module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff (awaitBody, runHalogenAff, HalogenEffects)
import Halogen.VDom.Driver (runUI)
import Svg.Hapykarva as HK

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI HK.hapykarva unit body
