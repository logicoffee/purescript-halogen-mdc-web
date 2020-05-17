module Drawer where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe (..))
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.MDC.Drawer as Drawer
import Halogen.MDC.List as List
import Halogen.MDC.ListItem as Item

import Route as Route

type State = Unit
data Action
  = Initialize
  | OnClick Route.Route
type Input = Unit
data Message
  = Clicked Route.Route

component :: H.Component HH.HTML (Const Void) Input Message Aff
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval evalSpec
  }
  where
    render :: State -> H.ComponentHTML Action () Aff
    render _ = Drawer.drawer Drawer.defaultProps
      [ List.list List.defaultProps
        [ Item.item Item.defaultProps
          { text = "Home"
          , onClick = const $ Just $ OnClick Route.Home
          }
        , Item.item Item.defaultProps
          { text = "Buttons"
          , onClick = const $ Just $ OnClick Route.Button
          }
        , Item.item Item.defaultProps
          { text = "Lists"
          , onClick = const $ Just $ OnClick Route.List
          }
        , Item.item Item.defaultProps
          { text = "Layout"
          , onClick = const $ Just $ OnClick Route.Layout
          }
        , Item.item Item.defaultProps
          { text = "card"
          , onClick = const $ Just $ OnClick Route.Card
          }
        ]
      ]

    evalSpec = H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }

    handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
    handleAction = case _ of
      Initialize -> pure unit
      OnClick route -> H.raise $ Clicked route
