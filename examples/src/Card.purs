module Card where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import CSS (paddingLeft, paddingRight, rem)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.MDC.Card as Card
import Halogen.MDC.Button as Button
import Halogen.MDC.Typography as Ty

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
      [ Card.primaryAction
        [ Card.media
          { shape: Card.Wide
          , url: "public/forest.jpg"
          }
          []
        , HH.div
          [ CSS.style do
            paddingLeft $ rem 1.0
            paddingRight $ rem 1.0
          ]
          [ Ty.typography { variant: Ty.H5, text: "Title" }
          , Ty.typography { variant: Ty.Body1, text: "lorem ipsum" }
          ]
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
