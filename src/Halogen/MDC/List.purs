module Halogen.MDC.List where

import Data.Array ((:))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Halogen.MDC.ListItem as Item

type Props =
  { lineNumber :: Item.LineNumber
  }

defaultProps :: Props
defaultProps =
  { lineNumber: Item.Single
  }

list :: forall w i. Props -> Array (HH.HTML w i) -> HH.HTML w i
list props = HH.ul [ HP.classes classes ] where
  classes =
    HH.ClassName "mdc-list"
    : case props.lineNumber of
        Item.Single -> []
        Item.Two -> [ HH.ClassName "mdc-list--two-line" ]
