module Main where

import Control.Monad.Random.Lazy (evalRandIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.IO as T
import Generators

main :: IO ()
main = do
  game <- evalRandIO generator
  BSL.putStrLn $ Aeson.encode game
