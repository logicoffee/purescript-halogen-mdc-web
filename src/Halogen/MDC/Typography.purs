module Halogen.MDC.Typography where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Props =
  { variant :: Variant
  , text :: String
  }

data Variant
  = H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | Subtitle1
  | Subtitle2
  | Body1
  | Body2
  | Caption
  | Button
  | Overline

typography :: forall w i. Props -> HH.HTML w i
typography props = elem [ HH.text props.text ]
  where
    elem = case props.variant of
      H1 -> HH.h1 [ className ]
      H2 -> HH.h2 [ className ]
      H3 -> HH.h3 [ className ]
      H4 -> HH.h4 [ className ]
      H5 -> HH.h5 [ className ]
      H6 -> HH.h6 [ className ]
      Subtitle1 -> HH.h6 [ className ]
      Subtitle2 -> HH.h6 [ className ]
      Body1 -> HH.p [ className ]
      Body2 -> HH.p [ className ]
      Caption -> HH.span [ className ]
      Button -> HH.span [ className ]
      Overline -> HH.span [ className ]

    className :: forall r. HP.IProp (class :: String | r) i
    className =
      HP.class_ $ HH.ClassName $ "mdc-typography--" <> case props.variant of
        H1 -> "headline1"
        H2 -> "headline2"
        H3 -> "headline3"
        H4 -> "headline4"
        H5 -> "headline5"
        H6 -> "headline6"
        Subtitle1 -> "subtitle1"
        Subtitle2 -> "subtitle2"
        Body1 -> "body1"
        Body2 -> "body2"
        Caption -> "caption"
        Button -> "button"
        Overline -> "overline"
