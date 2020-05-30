module Halogen.MDC.Button
  ( Props(..)
  , defaultProps
  , Variant(..)
  , IconProp(..)
  , button
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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

data Variant
  = Text
  | Raised
  | Unelevated
  | Outlined

instance showVariant :: Show Variant where
  show = case _ of
    Text -> ""
    Raised -> "raised"
    Unelevated -> "unelevated"
    Outlined -> "outlined"

data IconProp
  = Before String
  | After String

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
    <> case Tuple props.disabled props.theme of
      Tuple true _ -> []
      Tuple _ Th.Primary -> [ Th.cls.themeTextPrimary ]
      Tuple _ Th.Secondary -> [ Th.cls.themeTextSecondary ]
  Raised -> [ cls.button, cls.buttonRaised ]
    <> case Tuple props.disabled props.theme of
      Tuple true _ -> []
      Tuple _ t@Th.Primary -> [  Th.cls.themeBg t, Th.cls.themeTextOnPrimary ]
      Tuple _ t@Th.Secondary -> [ Th.cls.themeBg t, Th.cls.themeTextOnSecondary ]
  Unelevated -> [ cls.button, cls.buttonUnelevated ]
    <> case Tuple props.disabled props.theme of
      Tuple true _ -> []
      Tuple _ t@Th.Primary -> [ Th.cls.themeBg t, Th.cls.themeTextOnPrimary ]
      Tuple _ t@Th.Secondary -> [ Th.cls.themeBg t, Th.cls.themeTextOnSecondary ]
  Outlined -> [ cls.button, cls.buttonOutlined ]
    <> case Tuple props.disabled props.theme of
      Tuple true _ -> []
      Tuple _ Th.Primary -> [ Th.cls.themeTextPrimary ]
      Tuple _ Th.Secondary -> [ Th.cls.themeTextSecondary ]

cls ::
  { button :: HH.ClassName
  , buttonRipple :: HH.ClassName
  , buttonVariant :: Variant -> HH.ClassName
  , buttonRaised :: HH.ClassName
  , buttonUnelevated :: HH.ClassName
  , buttonOutlined :: HH.ClassName
  , buttonLabel :: HH.ClassName
  , buttonIcon :: HH.ClassName
  }
cls =
  { button: HH.ClassName "mdc-button"
  , buttonRipple: HH.ClassName "mdc-button__ripple"
  , buttonVariant: \variant -> HH.ClassName $ prefix <> "--" <> show variant
  , buttonRaised: HH.ClassName "mdc-button--raised"
  , buttonUnelevated: HH.ClassName "mdc-button--unelevated"
  , buttonOutlined: HH.ClassName "mdc-button--outlined"
  , buttonLabel: HH.ClassName "mdc-button__label"
  , buttonIcon: HH.ClassName "mdc-button__icon"
  }

prefix :: String
prefix = "mdc-button"
