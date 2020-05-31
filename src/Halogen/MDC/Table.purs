module Halogen.MDC.Table where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

table :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
table children = HH.div
  [ HP.class_ cls.table ]
  [ HH.div
    [ HP.class_ cls.tableContainer ]
    [ HH.div [ HP.class_ cls.tableTable ] children ]
  ]

header :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
header children = HH.thead_
  [ HH.tr [ HP.class_ cls.tableHeaderRow ] children ]

headerCell :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
headerCell = HH.th [ HP.class_ cls.tableHeaderCell ]

headerCellCheckbox :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
headerCellCheckbox = HH.th [ HP.classes [ cls.tableHeaderCell, cls.tableHeaderCellCheckbox ] ]

headerCellNumeric :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
headerCellNumeric = HH.th [ HP.classes [ cls.tableHeaderCell, cls.tableHeaderCellNumeric ] ]

body :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
body = HH.tbody [ HP.class_ cls.tableContent ]

row :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
row = HH.tr [ HP.class_ cls.tableRow ]

cell :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
cell = HH.td [ HP.class_ cls.tableCell ]

cellCheckbox :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
cellCheckbox = HH.td [ HP.classes [ cls.tableCell, cls.tableCellCheckbox ]  ]

cellNumeric :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
cellNumeric = HH.td [ HP.classes [ cls.tableCell, cls.tableHeaderCellNumeric ]  ]

cls ::
  { table :: HH.ClassName
  , tableContainer :: HH.ClassName
  , tableTable :: HH.ClassName
  , tableHeaderRow :: HH.ClassName
  , tableHeaderCell :: HH.ClassName
  , tableHeaderCellCheckbox :: HH.ClassName
  , tableHeaderCellNumeric :: HH.ClassName
  , tableContent :: HH.ClassName
  , tableRow :: HH.ClassName
  , tableCell :: HH.ClassName
  , tableCellCheckbox :: HH.ClassName
  , tableCellNumeric :: HH.ClassName
  , tableHeaderRowCheckbox :: HH.ClassName
  , tableRowCheckbox :: HH.ClassName
  , tableRowSelected :: HH.ClassName
  , tableHeaderCellSorted :: HH.ClassName
  , tableHeaderCellSortedDesc :: HH.ClassName
  , tableHeaderCellWithSort :: HH.ClassName
  , tableHeaderCellWrapper :: HH.ClassName
  , tableSortIconButton :: HH.ClassName
  , tableHeaderCellLabel :: HH.ClassName
  }

cls =
  { table: HH.ClassName "mdc-data-table"
  , tableContainer: HH.ClassName $ prefix <> "table-container"
  , tableTable: HH.ClassName $ prefix <> "table"
  , tableHeaderRow: HH.ClassName $ prefix <> "header-row"
  , tableHeaderCell: HH.ClassName $ prefix <> "header-cell"
  , tableHeaderCellCheckbox: HH.ClassName $ prefix <> "header-cell--checkbox"
  , tableHeaderCellNumeric: HH.ClassName $ prefix <> "header-cell--numeric"
  , tableContent: HH.ClassName $ prefix <> "content"
  , tableRow: HH.ClassName $ prefix <> "row"
  , tableCell: HH.ClassName $ prefix <> "cell"
  , tableCellCheckbox: HH.ClassName $ prefix <> "cell--checkbox"
  , tableCellNumeric: HH.ClassName $ prefix <> "cell--numeric"
  , tableHeaderRowCheckbox: HH.ClassName $ prefix <> "header-row-checkbox"
  , tableRowCheckbox: HH.ClassName $ prefix <> "row-checkbox"
  , tableRowSelected: HH.ClassName $ prefix <> "row--selected"
  , tableHeaderCellSorted: HH.ClassName $ prefix <> "header-cell--sorted"
  , tableHeaderCellSortedDesc: HH.ClassName $ prefix <> "header-cell--sorted-descending"
  , tableHeaderCellWithSort: HH.ClassName $ prefix <> "header-cell--with-sort"
  , tableHeaderCellWrapper: HH.ClassName $ prefix <> "header-cell-wrapper"
  , tableSortIconButton: HH.ClassName $ prefix <> "sort-icon-button"
  , tableHeaderCellLabel: HH.ClassName $ prefix <> "header-cell-label"
  }

prefix :: String
prefix = "mdc-data-table__"
