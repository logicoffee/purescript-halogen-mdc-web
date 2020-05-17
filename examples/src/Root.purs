module Root where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH

import Button as Button
import List as List
import Drawer as Drawer
import Route as Route
import Layout as Layout
import Card as Card
import Typography as Typography

data State = State Route.Route
data Action
  = Initialize
  | RouteChanged Route.Route

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: const $ State Route.Home
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render (State route) = HH.div_
  [ HH.slot (SProxy :: _ "drawer") unit Drawer.component unit handleDrawerMessages
  , case route of
      Route.Home -> HH.div_ [ HH.text "home" ]
      Route.Button -> HH.slot (SProxy :: _ "button") unit Button.component unit absurd
      Route.List -> HH.slot (SProxy :: _ "list") unit List.component unit absurd
      Route.Layout -> HH.slot (SProxy :: _ "layout") unit Layout.component unit absurd
      Route.Card -> HH.slot (SProxy :: _ "card") unit Card.component unit absurd
      Route.Typography -> HH.slot (SProxy :: _ "typography") unit Typography.component unit absurd
  ]

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction = case _ of
  Initialize -> pure unit
  RouteChanged route -> H.put $ State route

handleDrawerMessages :: Drawer.Message -> Maybe Action
handleDrawerMessages (Drawer.Clicked route) = Just $ RouteChanged route

type Slots =
  ( button :: SingleSlot
  , list :: SingleSlot
  , drawer :: H.Slot (Const Void) Drawer.Message Unit
  , layout :: SingleSlot
  , card :: SingleSlot
  , typography :: SingleSlot
  )

type SingleSlot = H.Slot (Const Void) Void Unit
