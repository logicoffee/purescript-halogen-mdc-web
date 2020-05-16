module Halogen.MDC.Grid where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Props =
  { align :: Align
  }

data Align
  = Center
  | Left
  | Right

defaultProps :: Props
defaultProps =
  { align: Center
  }

outer :: forall w i. Props -> Array (HH.HTML w i) -> HH.HTML w i
outer props = HH.div [ HP.class_ className ]
  where
    className = HH.ClassName case props.align of
      Center -> "mdc-layout-grid"
      Left -> "mdc-layout-grid--align-left"
      Right -> "mdc-layout-grid--align-right"

inner :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
inner = HH.div [ HP.class_ $ HH.ClassName "mdc-layout-grid__inner" ]

cell :: forall w i. Int -> Array (HH.HTML w i) -> HH.HTML w i
cell span = HH.div
  [ HP.classes $ map HH.ClassName [ "mdc-layout-grid__cell", "mdc-layout-grid__cell--span-" <> show span ]
  ]
