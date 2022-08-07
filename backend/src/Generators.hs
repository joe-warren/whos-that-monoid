{-# LANGUAGE GADTs #-}

module Generators (generator) where

import Control.Applicative (Alternative)
import Control.Arrow
import Control.Monad (guard, join, replicateM)
import Control.Monad.Random.Class
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import qualified Data.Bifunctor
import Data.Char (isNumber, toLower, toUpper)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheMonoids

showGenerator :: (Show a, Functor f) => f a -> f (Text, a)
showGenerator = fmap (T.pack . show &&& id)

intGenerator :: MonadRandom m => m (Text, Integer)
intGenerator = showGenerator $ getRandomR (0, 10)

stringGenerator :: MonadRandom m => m (Text, String)
stringGenerator = showGenerator $ uniform ["a", "b", "c", "abc", "cba", "A", "B", "C", "ABC", "CBA", "aBc", "CbA", "123", "1", "2", "3"]

boolGenerator :: MonadRandom m => m (Text, Bool)
boolGenerator = showGenerator $ getRandomR (True, False)

maybeGenerator :: MonadRandom m => m (Text, a) -> m (Text, Maybe a)
maybeGenerator gen =
  join $
    weighted
      [ (pure ("Nothing", Nothing), 1),
        (bimap ("Just " <>) Just <$> gen, 3)
      ]

stringEndoGenerator :: (MonadRandom m) => m (Text, String -> String)
stringEndoGenerator =
  uniform
    [ ("fmap toLower", fmap toLower),
      ("fmap toUpper", fmap toUpper),
      ("id", id),
      ("reverse", reverse)
    ]

comparisonGenerator :: (MonadRandom m, Ord a) => m (Text, a -> a -> Ordering)
comparisonGenerator =
  uniform
    [ ("compare", compare),
      ("flip compare", flip compare),
      ("const (const EQ)", const (const EQ))
    ]

intCompaisonGenerator :: (MonadRandom m) => m (Text, Integer -> Integer -> Ordering)
intCompaisonGenerator =
  join $
    uniform
      [ comparisonGenerator,
        do
          intValue <- getRandomR (2, 5)
          let name = "compare `on` (`mod` " <> (T.pack $ show intValue) <> ")"
          return (name, compare `on` (`mod` intValue))
      ]

stringCompaisonGenerator :: (MonadRandom m) => m (Text, String -> String -> Ordering)
stringCompaisonGenerator =
  join $
    uniform
      [ comparisonGenerator,
        pure ("compare `on` fmap toUpper", compare `on` fmap toUpper),
        pure ("compare `on` listToMaybe", compare `on` listToMaybe),
        pure ("compare `on` reverse", compare `on` reverse)
      ]

equivalenceGenerator :: (MonadRandom m, Eq a) => m (Text, a -> a -> Bool)
equivalenceGenerator =
  uniform
    [ ("(==)", (==)),
      ("const (const True)", const (const True))
    ]

intEquivalenceGenerator :: (MonadRandom m) => m (Text, Integer -> Integer -> Bool)
intEquivalenceGenerator =
  join $
    uniform
      [ equivalenceGenerator,
        do
          intValue <- getRandomR (2, 5)
          let name = "(==) `on` (`mod` " <> (T.pack . show $ intValue) <> ")"
          return (name, (==) `on` (`mod` intValue))
      ]

stringEquivalenceGenerator :: (MonadRandom m) => m (Text, String -> String -> Bool)
stringEquivalenceGenerator =
  join $
    uniform
      [ equivalenceGenerator,
        pure ("(==) `on` fmap toUpper", (==) `on` fmap toUpper),
        pure ("(==) `on` listToMaybe", (==) `on` listToMaybe),
        pure ("(==) `on` fmap isNumber", (==) `on` fmap isNumber),
        pure ("(==) `on` all isNumber", (==) `on` all isNumber)
      ]

tupleGenerator :: (Monad f) => f (Text, a) -> f (Text, b) -> f (Text, (a, b))
tupleGenerator fa fb =
  let tupleText ta tb = "(" <> ta <> ", " <> tb <> ")"
   in do
        (\((ta, a), (tb, b)) -> (tupleText ta tb, (a, b))) <$> ((,) <$> fa <*> fb)

pairGenerator :: (Monad f) => f (Text, a) -> f (Text, (a, a))
pairGenerator fa = tupleGenerator fa fa

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

data ComplexOutputFields = ComplexOutputFields
  { complexOutputInputs :: [Text],
    complexOutputOutputs :: [Text],
    complexOutputApplication :: Maybe Text
  }
  deriving (Generic)

instance ToJSON ComplexOutputFields

data Output = SimpleOutput Text | ComplexOutput ComplexOutputFields deriving (Generic)

instance ToJSON Output

data Game = Game
  { gameSolution :: MonoidName,
    gameOtherAnswers :: [MonoidName],
    gameInputs :: [Text],
    gameOutput :: Output,
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

data Application m a b c where
  SimpleApplication :: (a -> Text) -> Application m a a a
  ComplexApplication :: (m (Text, b)) -> (a -> b -> c) -> (c -> Text) -> Maybe Text -> Application m a b c

showApplication :: Show a => Application m a a a
showApplication = SimpleApplication (T.pack . show)

gameGenerator :: (MonadRandom m, Eq c) => m (Text, a) -> [MonoidInfo NonEmpty a] -> Application m a b c -> m Game
gameGenerator generator validMonoids application = do
  inputLength <- getRandomR (1, 4)
  unUniquedInputs <- (NE.:|) <$> generator <*> replicateM inputLength generator
  let inputs = NE.nubBy ((==) `on` fst) unUniquedInputs
  let rawInputs = snd <$> inputs
  MonoidInfo solutionName monoidKind sln <- uniform validMonoids

  (solver, output) <- case application of
    SimpleApplication f -> pure (pure . ($ rawInputs), SimpleOutput . f . sln $ rawInputs)
    ComplexApplication gb f showC applicationName -> do
      secondaryInputLength <- getRandomR (2, 5)
      unUniquedSecondaryInputs <- replicateM secondaryInputLength gb
      let secondaryInputs = nubBy ((==) `on` fst) unUniquedSecondaryInputs
      let genOutputs runner = f (runner rawInputs) . snd <$> secondaryInputs

      return (genOutputs, ComplexOutput $ ComplexOutputFields (fst <$> secondaryInputs) (showC <$> genOutputs sln) applicationName)

  let answer = solver sln
  let otherTypesafeSolutions = filter ((/= solutionName) . monoidName) validMonoids
  let candidateTypesafeSolutions = filter ((/= answer) . solver . runMonoid) otherTypesafeSolutions
  let invalidMonoids = filter (not . (`elem` (monoidName <$> validMonoids))) allNames
  let onlyOneTypesafeCandidate = null . tail $ validMonoids
  if null candidateTypesafeSolutions && not onlyOneTypesafeCandidate
    then gameGenerator generator validMonoids application
    else do
      wantedTypesafeSolutions <- let m = getRandomR (1, 3) in max <$> m <*> m
      selectedTypesafeSolutions <- upTo wantedTypesafeSolutions candidateTypesafeSolutions
      let wantedUnTypesafeSolutions = 3 - length selectedTypesafeSolutions
      untypesafeSolutions <- upTo wantedUnTypesafeSolutions invalidMonoids
      kind <- case monoidKind of
        OnlySemigroupal -> pure OnlySemigroupal
        AlsoMonoidal -> uniform [AlsoMonoidal, OnlySemigroupal]
      let aggregation = case kind of
            OnlySemigroupal -> "foldMap1"
            AlsoMonoidal -> "foldMap"

      return $
        Game
          { gameSolution = solutionName,
            gameOtherAnswers = (monoidName <$> selectedTypesafeSolutions) <> untypesafeSolutions,
            gameInputs = toList $ fst <$> inputs,
            gameOutput = output,
            gameAggregation = aggregation
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

comparisonNames :: [MonoidName]
comparisonNames = names (comparisonMonoids :: L (Int -> Int -> Ordering))

equivalenceNames :: [MonoidName]
equivalenceNames = names (equivalenceMonoids :: L (Int -> Int -> Bool))

endofunctionNames :: [MonoidName]
endofunctionNames = names (endofunctionMonoids :: L (Int -> Int))

maybeNames :: [MonoidName]
maybeNames = names (maybeMonoids :: L (Maybe Int))

alternativeNames :: [MonoidName]
alternativeNames = names (maybeMonoids :: L (Maybe Int))

applicativeMonoidNames :: [MonoidName]
applicativeMonoidNames = names (applicativeMonoidMonoids :: L (Maybe ()))

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
    <> comparisonNames
    <> equivalenceNames
    <> endofunctionNames
    <> maybeNames
    <> alternativeNames
    <> applicativeMonoidNames
    <> ordNames
    <> genericNames

--allMonoids = intMonoids <> boolMonoids <> nativeMonoids <> predicateMonoids <> endofunctionMonoids <> maybeMonoids <> ordSemigroups <> genericSemigroups

intGameGenerator :: (MonadRandom m) => m Game
intGameGenerator = gameGenerator intGenerator (intMonoids <> ordSemigroups <> genericSemigroups) showApplication

stringGameGenerator :: MonadRandom m => m Game
stringGameGenerator = gameGenerator stringGenerator (ordSemigroups <> genericSemigroups <> alternativeMonoids <> nativeMonoids) showApplication

maybeIntGameGenerator :: MonadRandom m => m Game
maybeIntGameGenerator = gameGenerator (maybeGenerator intGenerator) (maybeMonoids <> alternativeMonoids <> ordSemigroups <> genericSemigroups) showApplication

maybeStringGameGenerator :: MonadRandom m => m Game
maybeStringGameGenerator = gameGenerator (maybeGenerator stringGenerator) (maybeMonoids <> alternativeMonoids <> applicativeMonoidMonoids <> nativeMonoids <> ordSemigroups <> genericSemigroups) showApplication

boolGameGenerator :: MonadRandom m => m Game
boolGameGenerator = gameGenerator boolGenerator (boolMonoids <> ordSemigroups <> genericSemigroups) showApplication

predicateGameGenerator :: MonadRandom m => m Game
predicateGameGenerator =
  let predicateApplication = ComplexApplication intGenerator ($) (T.pack . show) Nothing
   in gameGenerator (predicateGenerator intGenerator) (predicateMonoids <> genericSemigroups) predicateApplication

endoBoolGameGenerator :: MonadRandom m => m Game
endoBoolGameGenerator =
  let predicateApplication = ComplexApplication boolGenerator ($) (T.pack . show) Nothing
   in gameGenerator (predicateGenerator boolGenerator) (predicateMonoids <> endofunctionMonoids <> genericSemigroups) predicateApplication

endoStringGameGenerator :: MonadRandom m => m Game
endoStringGameGenerator =
  let application = ComplexApplication stringGenerator ($) (T.pack . show) Nothing
   in gameGenerator stringEndoGenerator (endofunctionMonoids <> applicativeMonoidMonoids <> nativeMonoids <> genericSemigroups) application

uncurryName :: Maybe Text
uncurryName = Just "uncurry"

comparisonIntGameGenerator :: MonadRandom m => m Game
comparisonIntGameGenerator =
  let application = ComplexApplication (pairGenerator intGenerator) uncurry (T.pack . show) uncurryName
   in gameGenerator intCompaisonGenerator (comparisonMonoids <> applicativeMonoidMonoids <> nativeMonoids <> genericSemigroups) application

comparisonStringGameGenerator :: MonadRandom m => m Game
comparisonStringGameGenerator =
  let application = ComplexApplication (pairGenerator stringGenerator) uncurry (T.pack . show) uncurryName
   in gameGenerator stringCompaisonGenerator (comparisonMonoids <> applicativeMonoidMonoids <> nativeMonoids <> genericSemigroups) application

equivalenceIntGameGenerator :: MonadRandom m => m Game
equivalenceIntGameGenerator =
  let application = ComplexApplication (pairGenerator intGenerator) uncurry (T.pack . show) uncurryName
   in gameGenerator intEquivalenceGenerator (equivalenceMonoids <> genericSemigroups) application

equivalenceStringGameGenerator :: MonadRandom m => m Game
equivalenceStringGameGenerator =
  let application = ComplexApplication (pairGenerator stringGenerator) uncurry (T.pack . show) uncurryName
   in gameGenerator stringEquivalenceGenerator (equivalenceMonoids <> genericSemigroups) application

generator :: MonadRandom m => m Game
generator =
  join $
    uniform
      [ intGameGenerator,
        stringGameGenerator,
        maybeIntGameGenerator,
        maybeStringGameGenerator,
        predicateGameGenerator,
        endoBoolGameGenerator,
        endoStringGameGenerator,
        comparisonIntGameGenerator,
        comparisonStringGameGenerator,
        equivalenceIntGameGenerator,
        equivalenceStringGameGenerator
      ]