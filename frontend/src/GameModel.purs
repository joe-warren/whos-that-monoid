module GameModel where

import Data.List
import Data.Maybe
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
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as Rep
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign.Generic (class Decode, defaultOptions, genericDecode, genericDecodeJSON)

newtype MonoidName = MonoidName String


derive instance genericMonoidName :: Generic MonoidName _
instance decodeWithOptionsMonoidName :: Decode MonoidName where
  decode = genericDecode opts


data ComplexOutputFields = ComplexOutputFields
  { complexOutputInputs :: Array String,
    complexOutputOutputs :: Array String,
    complexOutputApplication :: Maybe String
  }


derive instance genericOutputFields :: Generic ComplexOutputFields _

instance decodeWithOptionsOutputFields :: Decode ComplexOutputFields where
  decode = genericDecode opts

data Output = SimpleOutput String | ComplexOutput ComplexOutputFields 

instance decodeWithOptionsOutput :: Decode Output where
  decode = genericDecode opts

derive instance genericOutput :: Generic Output _

data Game = Game
  { gameSolution :: MonoidName,
    gameOtherAnswers :: Array MonoidName,
    gameInputs :: Array String,
    gameOutput :: Output,
    gameAggregation :: String
  }


derive instance genericGame :: Generic Game _


instance decodeWithOptionsGame :: Decode Game where
  decode = genericDecode opts

opts = defaultOptions { unwrapSingleConstructors = true, unwrapSingleArguments = true }

noUnwrapOpts = defaultOptions { unwrapSingleConstructors = true, unwrapSingleArguments = true }

newtype Games = Games (Array Game)


derive instance genericGames :: Generic Games _

request :: String -> Aff (Either String Games)
request q = do
    response <- AX.get AXRF.string "data/0.json"
    pure $ case response of
            Left err -> Left $ AX.printError err
            Right res -> lmap show <<< runExcept $  genericDecodeJSON noUnwrapOpts $ res.body