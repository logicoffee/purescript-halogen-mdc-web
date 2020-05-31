module Halogen.MDC.Drawer where

import Prelude

import Data.Array ((:), catMaybes)
import Data.Maybe (Maybe(..))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Props =
  { variant :: Variant
  }

data Variant
  = Permanent
  | Dismissible
  | Modal

type Element = forall w i. Array (HH.HTML w i) -> HH.HTML w i

drawer :: forall w i. Variant -> Array (HH.HTML w i) -> HH.HTML w i
drawer variant = HH.aside [ HP.classes asideClasses ]
  where
    asideClasses = cls.drawer
      : case variant of
          Permanent -> []
          Dismissible -> [ cls.drawerDismissible ]
          Modal -> [ cls.drawerModal ]

type HeaderProps =
  { title :: String
  , subtitle :: Maybe String
  }

header :: forall w i. HeaderProps -> HH.HTML w i
header props =
  HH.div [ HP.class_ cls.drawerHeader ]
    $ catMaybes [ title, subtitle ]
  where
    title = Just $ HH.h3
      [ HP.class_ cls.drawerTitle ]
      [ HH.text props.title ]
    subtitle = case props.subtitle of
      Nothing -> Nothing
      Just text -> Just $ HH.h6
        [ HP.class_ cls.drawerSubtitle ]
        [ HH.text text ]

content :: Element
content = HH.div [ HP.class_ cls.drawerContent ]

cls ::
  { drawer :: HH.ClassName
  , drawerHeader :: HH.ClassName
  , drawerContent :: HH.ClassName
  , drawerTitle :: HH.ClassName
  , drawerSubtitle :: HH.ClassName
  , drawerDismissible :: HH.ClassName
  , drawerModal :: HH.ClassName
  , drawerOpen :: HH.ClassName
  , drawerOpening :: HH.ClassName
  , drawerClosing :: HH.ClassName
  , drawerAppContent :: HH.ClassName
  , drawerScrim :: HH.ClassName
  }

cls =
  { drawer: HH.ClassName prefix
  , drawerHeader: HH.ClassName $ prefix <> "__header"
  , drawerContent: HH.ClassName $ prefix <> "__content"
  , drawerTitle: HH.ClassName $ prefix <> "__title"
  , drawerSubtitle: HH.ClassName $ prefix <> "__subtitle"
  , drawerDismissible: HH.ClassName $ prefix <> "--dismissible"
  , drawerModal: HH.ClassName $ prefix <> "--modal"
  , drawerOpen: HH.ClassName $ prefix <> "--open"
  , drawerOpening: HH.ClassName $ prefix <> "--opening"
  , drawerClosing: HH.ClassName $ prefix <> "--closing"
  , drawerAppContent: HH.ClassName $ prefix <> "-app-content"
  , drawerScrim: HH.ClassName $ prefix <> "-scrim"
  }

prefix :: String
prefix = "mdc-drawer"
