module Halogen.MDC.Drawer where

import Prelude

import Data.Array ((:))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Props =
  { variant :: Variant
  }

data Variant
  = Permanent
  | Dismissible
  | Modal

defaultProps :: Props
defaultProps =
  { variant: Permanent
  }

drawer :: forall w i. Props -> Array (HH.HTML w i) -> HH.HTML w i
drawer props children = HH.aside
  [ HP.classes asideClasses ]
  [ HH.div
    [ HP.class_ $ HH.ClassName "mdc-drawer__content" ]
    children
  ]
  where
    asideClasses = HH.ClassName "mdc-drawer"
      : case props.variant of
          Permanent -> []
          Dismissible -> [ HH.ClassName "mdc-drawer--dismissible" ]
          Modal -> [ HH.ClassName "mdc-drawer--modal" ]
