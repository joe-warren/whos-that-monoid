module Main where

import Prelude

import App.IntroScreen as IntroScreen
import App.Quiz as Quiz
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random as Random
import GameModel (request)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.URL as URL
import Web.URL.URLSearchParams as URLParams

foreign import printLocation :: forall a. a -> String


getQuizId :: String -> Maybe String
getQuizId u = URLParams.get "game" =<< URL.searchParams <$> URL.fromAbsolute u


main :: Effect Unit
main = do
  w <- window
  pageUrl <- printLocation <$> location w
  randomGameId <- (show <<< floor) <$> Random.randomRange 0.0 1000.0
  HA.runHalogenAff $ 

    case getQuizId pageUrl of
      Nothing -> do
          body <- HA.awaitBody
          void $  runUI (IntroScreen.component randomGameId) unit body
      Just gid -> do
          r <- request gid
          case r of
            Left text -> liftEffect $ log text
            Right res -> do
                      body <- HA.awaitBody
                      component <- liftEffect $ Quiz.component (printLocation pageUrl) res
                      void $  runUI component unit body
