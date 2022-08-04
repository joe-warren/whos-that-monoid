module Main where

import GameModel
import Prelude

import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import App.Button as Button
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import Data.Argonaut.Core as JSON
import Data.Argonaut.Decode as JSON
import Data.Bifunctor (bimap, lmap)
import Data.Either (hush, Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign.Generic (defaultOptions, genericDecode, genericDecodeJSON)
import Halogen.Aff as HA
import Halogen.HTML (text)
import Halogen.VDom.Driver (runUI)
import Web.HTML.Event.EventTypes (offline)


main :: Effect Unit
main = HA.runHalogenAff do
  r <- request ""
  let t = case r of
            Left text -> text
            Right _ -> "Success"
  body <- HA.awaitBody
  runUI (Button.component t) unit body
