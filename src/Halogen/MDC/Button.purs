module Halogen.MDC.Button where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Array ((:), concat)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent (MouseEvent)

type Props a =
  { variant :: Variant
  , label :: String
  , icon :: Maybe IconProp
  , ripple :: Boolean
  , disabled :: Boolean
  , onClick :: MouseEvent -> Maybe a
  }

data Variant
  = Text
  | Raised
  | Unelevated
  | Outlined

data IconProp
  = Before String
  | After String

defaultProps :: forall a. Props a
defaultProps =
  { variant: Text
  , label: ""
  , icon: Nothing
  , ripple: true
  , disabled: false
  , onClick: const Nothing
  }

button :: forall w i. Props i -> HH.HTML w i
button props =
  HH.button
    [ HP.classes $ toClassNames props.variant
    , HE.onClick props.onClick
    , HP.disabled props.disabled
    ]
    $ concat
      [ if props.ripple
          then [ HH.div [ HP.class_ $ HH.ClassName "mdc-button__ripple" ] [] ]
          else []
      , content
      ]

  where
    toClassNames :: Variant -> Array HH.ClassName
    toClassNames variant =
      HH.ClassName "mdc-button"
      : case variant of
          Text -> []
          Raised -> [ HH.ClassName "mdc-button--raised" ]
          Unelevated -> [ HH.ClassName "mdc-button--unelevated" ]
          Outlined -> [ HH.ClassName "mdc-button--outlined" ]

    content :: Array (HH.HTML w i)
    content = case props.icon of
      Nothing -> [label]
      Just (Before iconName) -> [icon iconName, label]
      Just (After iconName) -> [label, icon iconName]
      where
        label = HH.span
          [ HP.class_ $ HH.ClassName "mdc-button__label" ]
          [ HH.text props.label ]
        icon = \iconName -> HH.i
          [ HP.class_ $ HH.ClassName "mdc-button__icon" ]
          [ HH.text iconName ]
