module Halogen.MDC.Grid
  ( Props(..)
  , defaultProps
  , CellProps(..)
  , defaultCellProps
  , Align(..)
  , Position(..)
  , Device(..)
  , outer
  , outerWith
  , inner
  , cell
  , cellWith
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (catMaybes)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Props =
  { align :: Align
  , fixedColumnWidth :: Boolean
  }

defaultProps :: Props
defaultProps =
  { align: Center
  , fixedColumnWidth: false
  }

type CellProps =
  { span :: Maybe Int
  , device :: Maybe Device
  , order :: Maybe Int
  , align :: Maybe Position
  }

defaultCellProps :: CellProps
defaultCellProps =
  { span: Nothing
  , device: Nothing
  , order: Nothing
  , align: Nothing
  }

data Align
  = Center
  | Left
  | Right

data Position
  = Top
  | Middle
  | Bottom

data Device
  = Desktop
  | Tablet
  | Phone

instance deviceShow :: Show Device where
  show = case _ of
    Desktop -> "desktop"
    Tablet -> "tablet"
    Phone -> "phone"

instance showAlign :: Show Align where
  show = case _ of
    Center -> "center"
    Left -> "left"
    Right -> "right"

instance showPosition :: Show Position where
  show = case _ of
    Top -> "top"
    Middle -> "middle"
    Bottom -> "bottom"

outer :: forall w i. Align -> Array (HH.HTML w i) -> HH.HTML w i
outer align = HH.div [ HP.class_ $ cls.gridAlign align ]

outerWith :: forall w i. Props -> Array (HH.HTML w i) -> HH.HTML w i
outerWith props = HH.div [ HP.classes $ catMaybes classes ] where
  classes =
    [ Just $ cls.gridAlign props.align
    , if props.fixedColumnWidth
        then Just cls.gridFixedColumnWidth
        else Nothing
    ]

inner :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
inner = HH.div [ HP.class_ $ cls.gridInner]

cell :: forall w i. Int -> Array (HH.HTML w i) -> HH.HTML w i
cell span = HH.div
  [ HP.classes [ cls.gridCell, cls.gridCellSpan span Nothing ] ]

cellWith :: forall w i. CellProps -> Array (HH.HTML w i) -> HH.HTML w i
cellWith props = HH.div [ HP.classes $ catMaybes classNames ] where
  classNames =
    [ case props.span of
        Nothing -> Nothing
        Just s -> Just $ cls.gridCellSpan s props.device
    , map cls.gridCellOrder props.order
    , map cls.gridCellAlign props.align
    ]

cls ::
  { gridAlign :: Align -> HH.ClassName
  , gridInner :: HH.ClassName
  , gridCell :: HH.ClassName
  , gridCellSpan :: Int -> Maybe Device -> HH.ClassName
  , gridCellOrder :: Int -> HH.ClassName
  , gridCellAlign :: Position -> HH.ClassName
  , gridFixedColumnWidth :: HH.ClassName
  }

cls =
  { gridAlign: \align -> HH.ClassName $ case align of
      Center -> "mdc-layout-grid"
      a -> "mdc-layout-grid-" <> show a
  , gridInner: HH.ClassName "mdc-layout-grid__inner"
  , gridCell: HH.ClassName "mdc-layout-grid__cell"
  , gridCellSpan: \span mDevice ->
      HH.ClassName $ "mdc-layout-grid__cell--span-" <> show span
      <> case mDevice of
        Nothing -> ""
        Just device -> show device
  , gridCellOrder: \order ->
      HH.ClassName $ "mdc-layout-grid__cell--order-" <> show order
  , gridCellAlign: \pos ->
      HH.ClassName $ "mdc-layout-grid__cell--align-" <> show pos
  , gridFixedColumnWidth: HH.ClassName "mdc-layout-grid--fixed-column-width"
  }
