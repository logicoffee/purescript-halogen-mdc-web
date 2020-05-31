module Layout where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import CSS (px, backgroundColor, height, gray)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.MDC.Grid as Grid

type State = Unit
data Action = Initialize
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
      [ Grid.outer Grid.Center
        [ Grid.inner
          [ Grid.cell 3 [ graySquare ]
          , Grid.cell 4 [ graySquare ]
          , Grid.cell 5 [ graySquare ]
          ]
        ]
      ]
    graySquare :: forall w i. HH.HTML w i
    graySquare = HH.div
      [ CSS.style do
          backgroundColor gray
          height $ px 30.0
      ]
      []
    evalSpec = H.defaultEval
