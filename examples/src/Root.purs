module Examples.Root where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH

import Examples.Button as Button
import Examples.Route as Route

data State = State Route.Route
data Action
  = Initialize
  | RouteChanged Route.Route

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: const $ State Route.Button
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render (State route) = case route of
  Route.Home -> HH.div_ [ HH.text "home" ]
  Route.Button -> HH.slot (SProxy :: _ "button") unit Button.component unit absurd

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction = case _ of
  Initialize -> pure unit
  RouteChanged route -> H.put $ State route

type Slots =
  ( button :: H.Slot (Const Void) Void Unit
  )
