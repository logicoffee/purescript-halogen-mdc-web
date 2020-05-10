module Examples.Button where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH

import Halogen.MDC.Button as Button

type State = Unit
data Action = Initialize
type Input = Unit
type Message = Void

component :: H.Component HH.HTML (Const Void) Input Message Aff
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ = HH.div_
  [ HH.slot _button 1 Button.button textButtonProps (const Nothing)
  , HH.slot _button 2 Button.button raisedButtonProps (const Nothing)
  , HH.slot _button 3 Button.button unelevatedButtonProps (const Nothing)
  , HH.slot _button 4 Button.button outlinedButtonProps (const Nothing)
  , HH.slot _button 5 Button.button disabledButtonProps (const Nothing)
  ]
  where
    textButtonProps = Button.defaultProps { label = "text button" }
    raisedButtonProps = Button.defaultProps
      { label = "raised button"
      , variant = Button.Raised
      }
    unelevatedButtonProps = Button.defaultProps
      { label = "unelevated button"
      , variant = Button.Unelevated
      }
    outlinedButtonProps = Button.defaultProps
      { label = "outlined button"
      , variant = Button.Outlined
      }
    disabledButtonProps = Button.defaultProps
      { label = "disabled button"
      , disabled = true
      }

type Slots =
  ( button :: H.Slot (Const Void) Button.Message Int
  )

_button = SProxy :: SProxy "button"

handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
handleAction = case _ of
  Initialize -> pure unit
