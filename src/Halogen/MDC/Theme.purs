module Halogen.MDC.Theme where

import Prelude

import Halogen.HTML as HH

data Theme
  = Primary
  | Secondary

instance showTheme :: Show Theme where
  show = case _ of
    Primary -> "primary"
    Secondary -> "secondary"

data TextStyle
  = PrimaryText
  | SecondaryText
  | Hint
  | Disabled
  | Icon

instance showTextStyle :: Show TextStyle where
  show = case _ of
    PrimaryText -> "primary"
    SecondaryText -> "secondary"
    Hint -> "hint"
    Disabled -> "disabled"
    Icon -> "icon"

-- see $property-values defined in https://github.com/material-components/material-components-web/blob/master/packages/mdc-theme/_variables.scss

cls ::
  { themeBg :: Theme -> HH.ClassName
  , themeTextPrimary :: HH.ClassName
  , themeTextSecondary :: HH.ClassName
  , themeTextBackground :: HH.ClassName
  , themeTextError :: HH.ClassName
  , themeTextOnPrimary :: HH.ClassName
  , themeTextOnSecondary :: HH.ClassName
  , themeTextOnSurface :: HH.ClassName
  , themeTextOnError :: HH.ClassName
  , themeTextOnBackground :: TextStyle -> HH.ClassName
  , themeTextOnLight :: TextStyle -> HH.ClassName
  , themeTextOnDark :: TextStyle -> HH.ClassName
  }

cls =
  { themeBg: \theme -> HH.ClassName $ prefix <> show theme <> "-bg"
  , themeTextPrimary: HH.ClassName $ prefix <> "primary"
  , themeTextSecondary: HH.ClassName $ prefix <> "secondary"
  , themeTextBackground: HH.ClassName $ prefix <> "background"
  , themeTextError: HH.ClassName $ prefix <> "error"
  , themeTextOnPrimary: HH.ClassName $ prefix <> "on-primary"
  , themeTextOnSecondary: HH.ClassName $ prefix <> "on-secondary"
  , themeTextOnSurface: HH.ClassName $ prefix <> "on-surface"
  , themeTextOnError: HH.ClassName $ prefix <> "on-error"
  , themeTextOnBackground: \style -> HH.ClassName $ prefix <> "text-" <> show style <> "-on-background"
  , themeTextOnLight: \style -> HH.ClassName $ prefix <> "text-" <> show style <> "-on-light"
  , themeTextOnDark: \style -> HH.ClassName $ prefix <> "text-" <> show style <> "-on-dark"
  }

prefix :: String
prefix = "mdc-theme--"
