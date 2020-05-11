module Halogen.MDC.ListItem where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent (MouseEvent)

type Props a =
  { lineNumber :: LineNumber
  , text :: String
  , secondaryText :: String
  , icon :: Maybe String
  , onClick :: MouseEvent -> Maybe a
  }

data LineNumber
  = Single
  | Two

defaultProps :: forall a. Props a
defaultProps =
  { lineNumber: Single
  , text: ""
  , secondaryText: ""
  , icon: Nothing
  , onClick: const Nothing
  }

item :: forall w i. Props i -> HH.HTML w i
item props = HH.li
  [ HP.class_ $ HH.ClassName "mdc-list-item"
  , HE.onClick props.onClick
  ]
  case props.icon of
    Nothing -> [ text ]
    Just iconName -> [ icon iconName, text ]

  where
    icon = \iconName -> HH.span
      [ HP.classes $
          map HH.ClassName ["mdc-list-item__graphic", "material-icons"]
      , ARIA.hidden "true"
      ]
      [ HH.text iconName ]
    text = HH.span
      [ HP.class_ $ HH.ClassName "mdc-list-item__text" ]
      case props.lineNumber of
        Single -> [ HH.text props.text ]
        Two ->
          [ HH.span
              [ HP.class_ $ HH.ClassName "mdc-list-item__primary-text" ]
              [ HH.text props.text ]
          , HH.span
              [ HP.class_ $ HH.ClassName "mdc-list-item__secondary-text" ]
              [ HH.text props.secondaryText ]
          ]
