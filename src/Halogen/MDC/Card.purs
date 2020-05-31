module Halogen.MDC.Card where

import Prelude

import CSS (backgroundImage, url)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as CSS
import Halogen.MDC.Internal as I

data Variant
  = Elevated
  | Outlined

data Shape
  = Square
  | Wide

instance showShape :: Show Shape where
  show = case _ of
    Square -> "square"
    Wide -> "16-9"

card :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
card = cardWith Elevated

cardWith :: forall w i. Variant -> Array (HH.HTML w i) -> HH.HTML w i
cardWith variant = HH.div [ HP.classes classNames ]
  where
    classNames = case variant of
      Elevated -> [ cls.card ]
      Outlined -> [ cls.card, cls.cardOutlined ]

primaryAction :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
primaryAction = HH.div [ HP.class_ cls.cardPrimaryAction ]

type MediaProps =
  { shape :: Shape
  , url :: String
  }

media :: MediaProps -> forall w i. Array (HH.HTML w i) -> HH.HTML w i
media props = HH.div
  [ HP.classes [ cls.cardMedia, cls.cardMediaShape props.shape ]
  , CSS.style $ backgroundImage $ url props.url
  ]

mediaContent :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mediaContent = HH.div [ HP.class_ cls.cardMediaContent ]

actions :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
actions = HH.div [ HP.class_ cls.cardActions ]

actionsFullBleed :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
actionsFullBleed = HH.div [ HP.classes [ cls.cardActions, cls.cardActionsFullBleed ] ]

actionButtons :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
actionButtons children = HH.div
  [ HP.class_ cls.cardActionButtons ]
  $ map
    (I.appendClassNames [ cls.cardAction, cls.cardActionButton ])
    children

actionIcons :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
actionIcons children = HH.div
  [ HP.class_ cls.cardActionIcons ]
  $ map
    (I.appendClassNames [ cls.cardAction, cls.cardActionIcon ])
    children

cls ::
  { card :: HH.ClassName
  , cardOutlined :: HH.ClassName
  , cardPrimaryAction :: HH.ClassName
  , cardMedia :: HH.ClassName
  , cardMediaShape :: Shape -> HH.ClassName
  , cardMediaContent :: HH.ClassName
  , cardActions :: HH.ClassName
  , cardActionsFullBleed :: HH.ClassName
  , cardActionButtons :: HH.ClassName
  , cardActionIcons :: HH.ClassName
  , cardAction :: HH.ClassName
  , cardActionButton :: HH.ClassName
  , cardActionIcon :: HH.ClassName
  }

cls =
  { card: HH.ClassName "mdc-card"
  , cardOutlined: HH.ClassName "mdc-card--outlined"
  , cardPrimaryAction: HH.ClassName $ prefix <> "primary-action"
  , cardMedia: HH.ClassName $ prefix <> "media"
  , cardMediaShape: \shape -> HH.ClassName $ prefix <> "media--" <> show shape
  , cardMediaContent: HH.ClassName $ prefix <> "media-content"
  , cardActions: HH.ClassName $ prefix <> "actions"
  , cardActionsFullBleed: HH.ClassName $ prefix <> "actions--full-bleed"
  , cardActionButtons: HH.ClassName $ prefix <> "actions-buttons"
  , cardActionIcons: HH.ClassName $ prefix <> "actions-icons"
  , cardAction: HH.ClassName $ prefix <> "action"
  , cardActionButton: HH.ClassName $ prefix <> "action--button"
  , cardActionIcon: HH.ClassName $ prefix <> "action--icon"
  }

prefix :: String
prefix = "mdc-card__"
