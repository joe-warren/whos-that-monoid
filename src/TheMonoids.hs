module TheMonoids where

import Control.Lens (Identity (..), Wrapped, ala, au, _Wrapping)
import Data.Aeson (ToJSON)
import Data.Functor.Contravariant (Predicate (..))
import qualified Data.Monoid
import qualified Data.Semigroup
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype MonoidName = MonoidName {getMonoidName :: Text} deriving (IsString, Eq, ToJSON) via Text

data IsMonoidal = OnlySemigroupal | AlsoMonoidal deriving (Eq, Generic)

instance ToJSON IsMonoidal

data MonoidInfo t a = MonoidInfo
  { monoidName :: MonoidName,
    monoidType :: IsMonoidal,
    runMonoid :: t a -> a
  }

intMonoids :: (Foldable t, Num a) => [MonoidInfo t a]
intMonoids =
  [ MonoidInfo "Sum" AlsoMonoidal (ala Data.Monoid.Sum foldMap),
    MonoidInfo "Product" AlsoMonoidal (ala Data.Monoid.Product foldMap)
  ]

ordSemigroups :: (Ord a, Foldable1 t) => [MonoidInfo t a]
ordSemigroups =
  [ MonoidInfo "Max" OnlySemigroupal (ala Data.Semigroup.Max foldMap1),
    MonoidInfo "Min" OnlySemigroupal (ala Data.Semigroup.Min foldMap1)
  ]

boolMonoids :: Foldable t => [MonoidInfo t Bool]
boolMonoids =
  [ MonoidInfo "Any" AlsoMonoidal (ala Data.Monoid.Any foldMap),
    MonoidInfo "All" AlsoMonoidal (ala Data.Monoid.All foldMap)
  ]

nativeMonoids :: (Foldable t, Monoid a) => [MonoidInfo t a]
nativeMonoids =
  [ MonoidInfo "Identity" AlsoMonoidal (ala Identity foldMap),
    MonoidInfo "Dual" AlsoMonoidal (ala Data.Monoid.Dual foldMap)
  ]

predicateMonoids :: (Foldable t) => [MonoidInfo t (a -> Bool)]
predicateMonoids = [MonoidInfo "Predicate" AlsoMonoidal (ala Predicate foldMap)]

endofunctionMonoids :: (Foldable t) => [MonoidInfo t (a -> a)]
endofunctionMonoids = [MonoidInfo "Endo" AlsoMonoidal (ala Data.Monoid.Endo foldMap)]

maybeMonoids :: Foldable t => [MonoidInfo t (Maybe a)]
maybeMonoids =
  [ MonoidInfo "Data.Monoid.First" AlsoMonoidal (ala Data.Monoid.First foldMap),
    MonoidInfo "Data.Monoid.Last" AlsoMonoidal (ala Data.Monoid.Last foldMap),
    MonoidInfo "Alt" AlsoMonoidal (ala Data.Monoid.Alt foldMap)
  ]

maybeMonoidMonoids :: (Foldable t, Monoid a) => [MonoidInfo t (Maybe a)]
maybeMonoidMonoids = [MonoidInfo "Ap" AlsoMonoidal (ala Data.Monoid.Ap foldMap)]

genericSemigroups :: Foldable1 t => [MonoidInfo t a]
genericSemigroups =
  [ MonoidInfo "Data.Semigroup.First" OnlySemigroupal (ala Data.Semigroup.First foldMap1),
    MonoidInfo "Data.Semigroup.Last" OnlySemigroupal (ala Data.Semigroup.Last foldMap1)
  ]
