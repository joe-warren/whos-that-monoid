module Main where

import Prelude

import App.Quiz as Quiz
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import GameModel (request)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window ( location)

foreign import printLocation :: forall a. a -> String

main :: Effect Unit
main = HA.runHalogenAff do
  r <- request ""
  case r of
    Left text -> liftEffect $ log text
    Right res -> do
              component <- liftEffect $ do 
                w <- window
                pageUrl <- location w
                Quiz.component (printLocation pageUrl) res
              body <- HA.awaitBody
              void $  runUI component unit body
