module Button where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Const (Const)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH

import Halogen.MDC.Button as Button

type State = Unit
data Action = Initialize
type Input = Unit
type Message = Void
type Slots = ()

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
  [ Button.button textButtonProps
  , Button.button raisedButtonProps
  , Button.button unelevatedButtonProps
  , Button.button outlinedButtonProps
  , Button.button disabledButtonProps
  , Button.button textButtoWithIconProps
  , Button.button raisedButtonWithIconProps
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
    textButtoWithIconProps = Button.defaultProps
      { label = "text button"
      , icon = Just $ Button.Before "favorite"
      }
    raisedButtonWithIconProps = Button.defaultProps
      { label = "raised button"
      , variant = Button.Raised
      , icon = Just $ Button.After "bookmark"
      }

handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
handleAction = case _ of
  Initialize -> pure unit
