module Button where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Const (Const)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH

import Halogen.MDC.Button as B
import Halogen.MDC.Typography as Ty
import Halogen.MDC.Theme as Th

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
render _ = HH.div_ $ map buttons
  [ B.Text
  , B.Raised
  , B.Unelevated
  , B.Outlined
  ]

buttons :: B.Variant -> forall w i. HH.HTML w i
buttons variant = HH.div_
  [ Ty.typography
    { variant: Ty.Subtitle1
    , text: case variant of
        B.Text -> "Text Button"
        B.Raised -> "Raised Button"
        B.Unelevated -> "Unelevated Button"
        B.Outlined -> "Outlined Button"
    }
  , B.button primaryProps
  , B.button secondaryProps
  , B.button withIconProps
  , B.button disabledProps
  ]
  where
    primaryProps = defaultProps
      { label = "default"
      , variant = variant
      }
    secondaryProps = defaultProps
      { label = "secondary"
      , variant = variant
      , theme = Th.Secondary
      }
    withIconProps = defaultProps
      { label = "icon"
      , variant = variant
      , icon = Just $ B.Before "favorite"
      }
    disabledProps = defaultProps
      { label = "disabled"
      , variant = variant
      , disabled = true
      }

handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
handleAction = case _ of
  Initialize -> pure unit

defaultProps :: forall a. B.Props a
defaultProps = B.defaultProps
  { extraClasses = [ HH.ClassName "demo-button" ]
  }
