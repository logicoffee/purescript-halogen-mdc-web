module Halogen.MDC.ListItem where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data State
  = State Input

data Action
  = Initialize
  | OnClick

type Input
  = Props

data Message
  = Clicked

type Props =
  { lineNumber :: LineNumber
  , text :: String
  , secondaryText :: String
  }

data LineNumber
  = Single
  | Two

item :: forall m. H.Component HH.HTML (Const Void) Input Message m
item = H.mkComponent
  { initialState: State
  , render
  , eval: H.mkEval evalSpec
  }
  where
  render :: State -> H.ComponentHTML Action () m
  render (State props) =
    HH.li
      [ HP.class_ $ HH.ClassName "mdc-list-item"
      , HE.onClick $ \_ -> Just OnClick
      ]
      [ HH.span
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
      ]

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Initialize -> pure unit
    OnClick -> H.raise Clicked

  evalSpec = H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
