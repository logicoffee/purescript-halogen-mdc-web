module Table where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.MDC.Table as T

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
  where
    render :: State -> H.ComponentHTML Action Slots Aff
    render _ = HH.div_
      [ T.table
        [ T.header
          [ T.headerCell [ HH.text "Dessert" ]
          , T.headerCell [ HH.text "Calories" ]
          , T.headerCell [ HH.text "Fat" ]
          , T.headerCell [ HH.text "Carbs" ]
          , T.headerCell [ HH.text "Protain (g)" ]
          ]
        , T.body
          [ T.row
            [ T.cell [ HH.text "Frozen yogurt" ]
            , T.cellNumeric [ HH.text "159" ]
            , T.cellNumeric [ HH.text "6" ]
            , T.cellNumeric [ HH.text "24" ]
            , T.cellNumeric [ HH.text "4" ]
            ]
          , T.row
            [ T.cell [ HH.text "Ice cream sandwich" ]
            , T.cellNumeric [ HH.text "237" ]
            , T.cellNumeric [ HH.text "9" ]
            , T.cellNumeric [ HH.text "37" ]
            , T.cellNumeric [ HH.text "4.3" ]
            ]
          ]
        ]
      ]

    handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
    handleAction = case _ of
      Initialize -> pure unit
