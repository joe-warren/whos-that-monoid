module Main where

import Control.Monad (replicateM)
import Control.Monad.Random.Lazy (evalRandIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.Text.IO as T
import Generators
import System.IO.Error.Lens (fileName)

genOne :: Int -> IO ()
genOne i = do
  print i
  game <- evalRandIO (replicateM 6 generator)
  let filename = "../frontend/static/data/" <> show i <> ".json"
  BSL.writeFile filename $ Aeson.encode game

main :: IO ()
main = traverse_ genOne [0 .. 1000]
