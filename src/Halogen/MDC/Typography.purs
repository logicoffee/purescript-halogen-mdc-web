module Halogen.MDC.Typography where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Props =
  { variant :: Variant
  , text :: String
  }

data Variant
  = Headline1
  | Headline2
  | Headline3
  | Headline4
  | Headline5
  | Headline6
  | Subtitle1
  | Subtitle2
  | Body1
  | Body2
  | Caption
  | Button
  | Overline

typography :: forall w i. Props -> HH.HTML w i
typography props = [ HH.text props.text ] # case props.variant of
  Headline1 -> HH.h1 [ toIProp Headline1 ]
  Headline2 -> HH.h2 [ toIProp Headline2 ]
  Headline3 -> HH.h3 [ toIProp Headline3 ]
  Headline4 -> HH.h4 [ toIProp Headline4 ]
  Headline5 -> HH.h5 [ toIProp Headline5 ]
  Headline6 -> HH.h6 [ toIProp Headline6 ]
  Subtitle1 -> HH.h6 [ toIProp Subtitle1 ]
  Subtitle2 -> HH.h6 [ toIProp Subtitle2 ]
  Body1 -> HH.p [ toIProp Body1 ]
  Body2 -> HH.p [ toIProp Body2 ]
  Caption -> HH.span [ toIProp Caption ]
  Button -> HH.span [ toIProp Button ]
  Overline -> HH.span [ toIProp Overline ]
  where
    toIProp :: forall r. Variant -> HP.IProp (class :: String | r) i
    toIProp variant =
      HP.class_ $ HH.ClassName $ "mdc-typography--" <> case variant of
        Headline1 -> "headline1"
        Headline2 -> "headline2"
        Headline3 -> "headline3"
        Headline4 -> "headline4"
        Headline5 -> "headline5"
        Headline6 -> "headline6"
        Subtitle1 -> "subtitle1"
        Subtitle2 -> "subtitle2"
        Body1 -> "body1"
        Body2 -> "body2"
        Caption -> "caption"
        Button -> "button"
        Overline -> "overline"
