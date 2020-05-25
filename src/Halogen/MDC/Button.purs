module Halogen.MDC.Button where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Tuple.Nested ((/\))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent (MouseEvent)

import Halogen.MDC.Theme as Th

type Props a =
  { variant :: Variant
  , label :: String
  , icon :: Maybe IconProp
  , ripple :: Boolean
  , disabled :: Boolean
  , onClick :: MouseEvent -> Maybe a
  , theme :: Th.Theme
  , extraClasses :: Array HH.ClassName
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
  , theme: Th.Primary
  , extraClasses: []
  }

button :: forall w i. Props i -> HH.HTML w i
button props =
  HH.button
    [ HP.classes $ rootClassNames props <> props.extraClasses
    , HE.onClick props.onClick
    , HP.disabled props.disabled
    ]
    $ ripple <> content

  where
    ripple :: Array (HH.HTML w i)
    ripple = if props.ripple
      then [ HH.div [ HP.class_ cls.buttonRipple ] [] ]
      else []

    content :: Array (HH.HTML w i)
    content = case props.icon of
      Nothing -> [label]
      Just (Before iconName) -> [icon iconName, label]
      Just (After iconName) -> [label, icon iconName]

    label = HH.span
      [ HP.classes [ cls.buttonLabel ] ]
      [ HH.text props.label ]
    icon = \iconName -> HH.i
      [ HP.classes [ cls.buttonIcon, HH.ClassName "material-icons" ]
      , ARIA.hidden "true"
      ]
      [ HH.text iconName ]

rootClassNames :: forall a. Props a -> Array HH.ClassName
rootClassNames props = case props.variant of
  Text -> [ cls.button ]
    <> case props.disabled /\ props.theme of
      true /\ _ -> []
      _ /\ Th.Primary -> [ Th.cls.themePrimary ]
      _ /\ Th.Secondary -> [ Th.cls.themeSecondary ]
  Raised -> [ cls.button, cls.buttonRaised ]
    <> case props.disabled /\ props.theme of
      true /\ _ -> []
      _ /\ Th.Primary -> [  Th.cls.themePrimaryBg, Th.cls.themeOnPrimary ]
      _ /\ Th.Secondary -> [ Th.cls.themeSecondaryBg, Th.cls.themeOnSecondary ]
  Unelevated -> [ cls.button, cls.buttonUnelevated ]
    <> case props.disabled /\ props.theme of
      true /\ _ -> []
      _ /\ Th.Primary -> [ Th.cls.themePrimaryBg, Th.cls.themeOnPrimary ]
      _ /\ Th.Secondary -> [ Th.cls.themeSecondaryBg, Th.cls.themeOnSecondary ]
  Outlined -> [ cls.button, cls.buttonOutlined ]
    <> case props.disabled /\ props.theme of
      true /\ _ -> []
      _ /\ Th.Primary -> [ Th.cls.themePrimary ]
      _ /\ Th.Secondary -> [ Th.cls.themeSecondary ]

cls ::
  { button :: HH.ClassName
  , buttonRipple :: HH.ClassName
  , buttonRaised :: HH.ClassName
  , buttonUnelevated :: HH.ClassName
  , buttonOutlined :: HH.ClassName
  , buttonLabel :: HH.ClassName
  , buttonIcon :: HH.ClassName
  }
cls =
  { button: HH.ClassName "mdc-button"
  , buttonRipple: HH.ClassName "mdc-button__ripple"
  , buttonRaised: HH.ClassName "mdc-button--raised"
  , buttonUnelevated: HH.ClassName "mdc-button--unelevated"
  , buttonOutlined: HH.ClassName "mdc-button--outlined"
  , buttonLabel: HH.ClassName "mdc-button__label"
  , buttonIcon: HH.ClassName "mdc-button__icon"
  }
