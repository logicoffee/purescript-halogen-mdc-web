module Halogen.MDC.Internal
  ( appendClassNames
  ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Array (uncons)
import Data.String (Pattern (..), split, joinWith)

import Halogen.HTML as HH
import Halogen.VDom.Types as VDom
import Halogen.VDom.DOM.Prop as DP
import Unsafe.Coerce (unsafeCoerce)

appendClassNames :: forall w i. Array HH.ClassName -> HH.HTML w i -> HH.HTML w i
appendClassNames classNames (HH.HTML vdom) = HH.HTML case vdom of
  VDom.Elem ns n props c -> VDom.Elem ns n (appendClassNameToProps classNames props) c
  VDom.Keyed ns n props c -> VDom.Keyed ns n (appendClassNameToProps classNames props) c
  other -> other

appendClassNameToProps :: forall a. Array HH.ClassName -> Array (DP.Prop a) -> Array (DP.Prop a)
appendClassNameToProps classNames props =
  props <> [ DP.Property "className" $ DP.propFromString $ joinWith " " $ append (map classNameToString classNames) $ getClassNames props ]

classNameToString :: HH.ClassName -> String
classNameToString (HH.ClassName className) = className

getClassNames :: forall a. Array (DP.Prop a) -> Array String
getClassNames props = case getClassPropValue props of
  Just propValue -> propValueToClassNames propValue
  Nothing -> []

getClassPropValue :: forall a. Array (DP.Prop a) -> Maybe DP.PropValue
getClassPropValue ps = case uncons ps of
  Just { head, tail } -> case head of
    DP.Property "className" propValue -> Just propValue
    _ -> getClassPropValue tail
  Nothing -> Nothing

propValueToClassNames :: DP.PropValue -> Array String
propValueToClassNames propValue =
  split (Pattern " ") $ unsafeCoerce propValue
