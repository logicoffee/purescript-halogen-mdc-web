module Halogen.MDC.Button where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Array ((:), concat)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data State
  = State Props

data Action
  = Initialize
  | OnClick

type Input
  = Props

data Message
  = Clicked

type Props =
  { variant :: Variant
  , label :: String
  , icon :: Maybe IconProp
  , ripple :: Boolean
  , disabled :: Boolean
  }

data Variant
  = Text
  | Raised
  | Unelevated
  | Outlined

data IconProp
  = Before String
  | After String

defaultProps :: Props
defaultProps =
  { variant: Text
  , label: ""
  , icon: Nothing
  , ripple: true
  , disabled: false
  }

button :: forall m. H.Component HH.HTML (Const Void) Input Message m
button = H.mkComponent
  { initialState: State
  , render
  , eval: H.mkEval evalSpec
  }
  where
  render :: State -> H.ComponentHTML Action () m
  render (State props) = HH.button
    [ HE.onClick $ \_ -> Just OnClick
    , HP.classes $ toClassNames props.variant
    , HP.disabled props.disabled
    ]
    $ concat
      [ if props.ripple
          then [ HH.div [ HP.class_ $ HH.ClassName "mdc-button__ripple" ] [] ]
          else []
      , renderContents props
      ]

  evalSpec = H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Initialize -> pure unit
    OnClick -> H.raise Clicked

  renderContents :: Props -> Array (H.ComponentHTML Action () m)
  renderContents props = case props.icon of
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

toClassNames :: Variant -> Array HH.ClassName
toClassNames variant =
  HH.ClassName "mdc-button"
  : case variant of
      Text -> []
      Raised -> [ HH.ClassName "mdc-button--raised" ]
      Unelevated -> [ HH.ClassName "mdc-button--unelevated" ]
      Outlined -> [ HH.ClassName "mdc-button--outlined" ]
