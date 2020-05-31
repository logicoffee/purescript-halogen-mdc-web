module List where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Const (Const)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH

import Halogen.MDC.List as List

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
      [ HH.div_
        [ List.list List.Single
          [ List.item $ List.defaultProps
            { text = "first element"
            }
          , List.item $ List.defaultProps
            { text = "second element"
            }
          ]
        ]
      , HH.div_
        [ List.list List.Single
          [ List.item $ List.defaultProps
            { text = "first element"
            , icon = Just "favorite"
            }
          , List.item $ List.defaultProps
            { text = "second element"
            , icon = Just "bookmark"
            }
          ]
        ]
      ]
    evalSpec = H.defaultEval
