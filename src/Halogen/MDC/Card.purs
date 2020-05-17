module Halogen.MDC.Card where

import Prelude

import Data.Array ((:))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.MDC.Internal as I

type Props =
  { variant :: Variant
  }

data Variant
  = Elevated
  | Outlined

defaultProps :: Props
defaultProps =
  { variant: Elevated
  }

card :: forall w i. Props -> Array (HH.HTML w i) -> HH.HTML w i
card props = HH.div [ HP.classes classNames ]
  where
    classNames = map HH.ClassName $ "mdc-card"
      : case props.variant of
          Elevated -> []
          Outlined -> [ "mdc-card--outlined" ]

primaryAction :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
primaryAction = HH.div [ HP.class_ $ HH.ClassName "mdc-card__primary-action" ]

media :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
media = HH.div [ HP.class_ $ HH.ClassName "mdc-card__media" ]

actions :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
actions = HH.div [ HP.class_ $ HH.ClassName "mdc-card__actions" ]

actionButtons :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
actionButtons children = HH.div
  [ HP.class_ $ HH.ClassName "mdc-card__action-buttons" ]
  $ map
    (I.appendClassName [ "mdc-card__action", "mdc-card__action--button" ])
    children

actionIcons :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
actionIcons children = HH.div
  [ HP.class_ $ HH.ClassName "mdc-card__action-icons" ]
  $ map
    (I.appendClassName [ "mdc-card__action", "mdc-card__action--icon" ])
    children
