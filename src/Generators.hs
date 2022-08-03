module Generators (generator) where

import Control.Applicative (Alternative)
import Control.Arrow
import Control.Monad (guard, join, replicateM)
import Control.Monad.Random.Class
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import qualified Data.Bifunctor
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheMonoids

showGenerator :: (Show a, Functor f) => f a -> f (Text, a)
showGenerator = fmap (T.pack . show &&& id)

intGenerator :: MonadRandom m => m (Text, Integer)
intGenerator = showGenerator $ getRandomR (0, 10)

stringGenerator :: MonadRandom m => m (Text, Text)
stringGenerator = showGenerator $ uniform ["a", "b", "c", "abc", "cba", "123", "1", "2", "3"]

boolGenerator :: MonadRandom m => m (Text, Bool)
boolGenerator = showGenerator $ getRandomR (True, False)

maybeGenerator :: MonadRandom m => m (Text, a) -> m (Text, Maybe a)
maybeGenerator gen =
  join $
    weighted
      [ (pure ("Nothing", Nothing), 1),
        (bimap ("Just " <>) Just <$> gen, 3)
      ]

predicateGenerator :: (MonadRandom m, Ord a) => m (Text, a) -> m (Text, a -> Bool)
predicateGenerator gen = do
  (opName, opFn) <-
    weighted
      [ (("==", (==)), 4),
        (("/=", (/=)), 1),
        (("<", (<)), 1),
        (("<=", (<=)), 1),
        ((">", (>)), 1),
        ((">=", (>=)), 1)
      ]
  let foo = (10 <=)
  (valName, val) <- gen
  return ("(" <> valName <> " " <> opName <> ")", opFn val)

data Game = Game
  { gameSolution :: MonoidName,
    gameOtherAnswers :: [MonoidName],
    gameInputs :: [Text],
    gameOutput :: Text,
    gameAggregation :: Text
  }
  deriving (Generic)

instance ToJSON Game

upTo :: MonadRandom m => Int -> [a] -> m [a]
upTo 0 _ = pure []
upTo _ [] = pure []
upTo n xs = do
  i <- getRandomR (0, length xs - 1)
  let (front, x : back) = splitAt i xs
  (x :) <$> upTo (n - 1) (front <> back)

gameGenerator :: (MonadRandom m, Eq a) => m (Text, a) -> [MonoidInfo NonEmpty a] -> (a -> Text) -> m Game
gameGenerator generator validMonoids showFn = do
  inputLength <- getRandomR (1, 4)
  inputs <- (NE.:|) <$> generator <*> replicateM inputLength generator
  let rawInputs = snd <$> inputs
  MonoidInfo solutionName monoidKind sln <- uniform validMonoids
  let answer = sln rawInputs
  let otherTypesafeSolutions = filter ((/= solutionName) . monoidName) validMonoids
  let candidateTypesafeSolutions = filter ((/= answer) . ($ rawInputs) . runMonoid) otherTypesafeSolutions
  let invalidMonoids = filter (not . (`elem` (monoidName <$> validMonoids))) allNames
  let onlyOneTypesafeCandidate = null . tail $ validMonoids
  if null candidateTypesafeSolutions && not onlyOneTypesafeCandidate
    then gameGenerator generator validMonoids showFn
    else do
      wantedTypesafeSolutions <- let m = getRandomR (1, 3) in max <$> m <*> m
      selectedTypesafeSolutions <- upTo wantedTypesafeSolutions candidateTypesafeSolutions
      let wantedUnTypesafeSolutions = 3 - length selectedTypesafeSolutions
      untypesafeSolutions <- upTo wantedUnTypesafeSolutions invalidMonoids
      kind <- case monoidKind of
        OnlySemigroupal -> pure OnlySemigroupal
        AlsoMonoidal -> uniform [AlsoMonoidal, OnlySemigroupal]
      let agregation = case kind of
            OnlySemigroupal -> "foldMap1"
            AlsoMonoidal -> "foldMap"

      return $
        Game
          { gameSolution = solutionName,
            gameOtherAnswers = (monoidName <$> selectedTypesafeSolutions) <> untypesafeSolutions,
            gameInputs = toList $ fst <$> inputs,
            gameOutput = showFn answer,
            gameAggregation = "foldmap"
          }

names :: [MonoidInfo NonEmpty a] -> [MonoidName]
names = fmap monoidName

intNames :: [MonoidName]
intNames = names intMonoids

boolNames :: [MonoidName]
boolNames = names boolMonoids

type L a = [MonoidInfo NonEmpty a]

nativeNames :: [MonoidName]
nativeNames = names (nativeMonoids :: L ())

predicateNames :: [MonoidName]
predicateNames = names (predicateMonoids :: L (Int -> Bool))

endofunctionNames :: [MonoidName]
endofunctionNames = names (endofunctionMonoids :: L (Int -> Int))

maybeNames :: [MonoidName]
maybeNames = names (maybeMonoids :: L (Maybe Int))

maybeMonoidNames :: [MonoidName]
maybeMonoidNames = names (maybeMonoidMonoids :: L (Maybe ()))

ordNames :: [MonoidName]
ordNames = names (ordSemigroups :: L ())

genericNames :: [MonoidName]
genericNames = names (genericSemigroups :: L ())

allNames :: [MonoidName]
allNames =
  intNames
    <> boolNames
    <> nativeNames
    <> predicateNames
    <> endofunctionNames
    <> maybeNames
    <> ordNames
    <> genericNames

--allMonoids = intMonoids <> boolMonoids <> nativeMonoids <> predicateMonoids <> endofunctionMonoids <> maybeMonoids <> ordSemigroups <> genericSemigroups

intGameGenerator :: (MonadRandom m) => m Game
intGameGenerator = gameGenerator intGenerator (intMonoids <> ordSemigroups <> genericSemigroups) (T.pack . show)

stringGameGenerator :: MonadRandom m => m Game
stringGameGenerator = gameGenerator stringGenerator (ordSemigroups <> genericSemigroups <> nativeMonoids) (T.pack . show)

maybeIntGameGenerator :: MonadRandom m => m Game
maybeIntGameGenerator = gameGenerator (maybeGenerator intGenerator) (maybeMonoids <> ordSemigroups <> genericSemigroups) (T.pack . show)

maybeStringGameGenerator :: MonadRandom m => m Game
maybeStringGameGenerator = gameGenerator (maybeGenerator stringGenerator) (maybeMonoids <> maybeMonoidMonoids <> nativeMonoids <> ordSemigroups <> genericSemigroups) (T.pack . show)

boolGameGenerator :: MonadRandom m => m Game
boolGameGenerator = gameGenerator boolGenerator (boolMonoids <> ordSemigroups <> genericSemigroups) (T.pack . show)

--predicateGameGenerator :: MonadRandom

generator :: MonadRandom m => m Game
generator =
  join $
    uniform
      [ intGameGenerator,
        stringGameGenerator,
        maybeIntGameGenerator,
        maybeStringGameGenerator
      ]