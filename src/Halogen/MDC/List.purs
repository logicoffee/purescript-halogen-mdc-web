module Halogen.MDC.List where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array ((:))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as ARIA
import Web.UIEvent.MouseEvent (MouseEvent)

list :: forall w i. LineNumber -> Array (HH.HTML w i) -> HH.HTML w i
list lineNumber = HH.ul [ HP.classes classes ] where
  classes = cls.list
    : case lineNumber of
        Single -> []
        Two -> [ cls.listTwoLine ]

type Props a =
  { lineNumber :: LineNumber
  , text :: String
  , secondaryText :: String
  , icon :: Maybe String
  , onClick :: MouseEvent -> Maybe a
  }

data LineNumber
  = Single
  | Two

defaultProps :: forall a. Props a
defaultProps =
  { lineNumber: Single
  , text: ""
  , secondaryText: ""
  , icon: Nothing
  , onClick: const Nothing
  }

item :: forall w i. Props i -> HH.HTML w i
item props = HH.li
  [ HP.class_ cls.listItem
  , HE.onClick props.onClick
  ]
  case props.icon of
    Nothing -> [ text ]
    Just iconName -> [ icon iconName, text ]

  where
    icon = \iconName -> HH.span
      [ HP.classes [ cls.listItemGraphic, HH.ClassName "material-icons"]
      , ARIA.hidden "true"
      ]
      [ HH.text iconName ]
    text = HH.span
      [ HP.class_ cls.listItemText ]
      case props.lineNumber of
        Single -> [ HH.text props.text ]
        Two ->
          [ HH.span
              [ HP.class_ cls.listItemPrimaryText ]
              [ HH.text props.text ]
          , HH.span
              [ HP.class_ cls.listItemSecondaryText ]
              [ HH.text props.secondaryText ]
          ]

divider :: forall w i. HH.HTML w i
divider = HH.li [ ARIA.role "separator", HP.class_ cls.listDivider ] []

group :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
group = HH.div [ HP.class_ cls.listGroup ]

groupSubheader :: forall w i. String -> HH.HTML w i
groupSubheader text = HH.h3 [ HP.class_ cls.listGroupSubheader ] [ HH.text text ]


cls ::
  { list :: HH.ClassName
  , listDense :: HH.ClassName
  , listAvatar :: HH.ClassName
  , listTwoLine :: HH.ClassName
  , listItem :: HH.ClassName
  , listItemText :: HH.ClassName
  , listItemPrimaryText :: HH.ClassName
  , listItemSecondaryText :: HH.ClassName
  , listItemDisabled :: HH.ClassName
  , listItemSelected :: HH.ClassName
  , listItemActivated :: HH.ClassName
  , listItemGraphic :: HH.ClassName
  , listItemMeta :: HH.ClassName
  , listGroup :: HH.ClassName
  , listGroupSubheader :: HH.ClassName
  , listDivider :: HH.ClassName
  , listDividerPadded :: HH.ClassName
  , listDividerInset :: HH.ClassName
  }

cls =
  { list: HH.ClassName prefix
  , listDense: HH.ClassName $ prefix <> "--dense"
  , listAvatar: HH.ClassName $ prefix <> "--avatar-list"
  , listTwoLine: HH.ClassName $ prefix <> "--two-line"
  , listItem: HH.ClassName $ prefix <> "-item"
  , listItemText: HH.ClassName $ prefix <> "-item__text"
  , listItemPrimaryText: HH.ClassName $ prefix <> "-item__primary-text"
  , listItemSecondaryText: HH.ClassName $ prefix <> "-item__secondary-text"
  , listItemDisabled: HH.ClassName $ prefix <> "-item--disabled"
  , listItemSelected: HH.ClassName $ prefix <> "-item--selected"
  , listItemActivated: HH.ClassName $ prefix <> "-item--activated"
  , listItemGraphic: HH.ClassName $ prefix <> "-item__graphic"
  , listItemMeta: HH.ClassName $ prefix <> "-item__meta"
  , listGroup: HH.ClassName $ prefix <> "-group"
  , listGroupSubheader: HH.ClassName $ prefix <> "-group__subheader"
  , listDivider: HH.ClassName $ prefix <> "-divider"
  , listDividerPadded: HH.ClassName $ prefix <> "-divider--padded"
  , listDividerInset: HH.ClassName $ prefix <> ""
  }

prefix :: String
prefix = "mdc-list"
