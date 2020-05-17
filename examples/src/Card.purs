module Card where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.MDC.Card as Card
import Halogen.MDC.Button as Button

type State = Unit
data Action
  = Initialize
type Input = Unit
type Message = Void

component :: H.Component HH.HTML (Const Void) Input Message Aff
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval evalSpec
  }
  where
    render :: State -> H.ComponentHTML Action () Aff
    render _ = HH.div_
      [ textCard
      ]

    textCard = Card.card
      Card.defaultProps
      [ Card.primaryAction
        [ HH.h2_ [ HH.text "Title" ]
        , HH.div_ [ HH.text "lorem ipsum" ]
        ]
      , Card.actions
        [ Card.actionButtons
          [ Button.button Button.defaultProps
            { label = "read"
            }
          , Button.button Button.defaultProps
            { label = "bookmark"
            }
          ]
        ]
      ]

    evalSpec = H.defaultEval
