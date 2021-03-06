module Main where

import Prelude
import Effect (Effect)

import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import Root as Root

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI Root.component unit body
