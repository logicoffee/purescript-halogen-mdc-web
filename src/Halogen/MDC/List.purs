module Halogen.MDC.List where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Array ((:), concat)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDC.ListItem as Item

data State
  = State Input

data Action
  = Initialize

type Input
  = Props

type Message
  = Void

type Props =
  { lineNumber :: Item.LineNumber
  }

defaultProps :: Props
defaultProps =
  { lineNumber: Item.Single
  }

{-- list :: forall m. H.Component HH.HTML (Const Void) Input Message m --}
{-- list = H.mkComponent --}
{--   { initialState: State --}
{--   , render --}
{--   , eval: H.mkEval evalSpec --}
{--   } --}
{--   where --}
{--     render :: State -> H.ComponentHTML Action () m --}
{--     render --}
