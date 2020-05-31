module Typography where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.MDC.Typography as T

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
      [ T.typography { variant: T.H1, text: "Headline1" }
      , T.typography { variant: T.H2, text: "Headline2" }
      , T.typography { variant: T.H3, text: "Headline3" }
      , T.typography { variant: T.H4, text: "Headline4" }
      , T.typography { variant: T.H5, text: "Headline5" }
      , T.typography { variant: T.H6, text: "Headline6" }
      , T.typography { variant: T.Subtitle1, text: "Subtitle1" }
      , T.typography { variant: T.Subtitle2, text: "Subtitle2" }
      , T.typography { variant: T.Body1, text: "Body1" }
      , T.typography { variant: T.Body2, text: "Body2" }
      , T.typography { variant: T.Caption, text: "Caption" }
      , T.typography { variant: T.Button, text: "Button" }
      , T.typography { variant: T.Overline, text: "Overline" }
      ]

    evalSpec = H.defaultEval
