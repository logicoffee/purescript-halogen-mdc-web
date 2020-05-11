module Examples.List where

import Prelude
import Data.Const (Const)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH

import Halogen.MDC.List as List
import Halogen.MDC.ListItem as Item

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
      [ List.list
          List.defaultProps
          [ Item.item $ Item.defaultProps
              { text = "first element"
              }
          , Item.item $ Item.defaultProps
              { text = "second element"
              }
          ]
      ]
    evalSpec = H.defaultEval
