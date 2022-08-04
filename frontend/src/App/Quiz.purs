module App.Quiz where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random as Random
import Foreign.NullOrUndefined (undefined)
import GameModel (Output(..))
import GameModel as GM
import Halogen as H
import Halogen.HTML (output)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Partial.Unsafe (unsafePartial)
import Web.HTML.Event.EventTypes (offline)


--data RoundState = Win | Loose | InPlay

type Answer = { answerText :: GM.MonoidName, 
      answerCorrect :: Boolean,
      answerClicked :: Boolean
    }

type Round cs m = {
    quizLine :: HH.ComponentHTML Action cs m, 
    answers :: Array Answer
  }

type State cs m 
  = { previousRounds :: Array (Round cs m),
      currentRound :: Maybe (Round cs m), 
      upcomingRounds :: Array (Round cs m)
    }

shuffle :: ∀ a. Array a -> Effect (Array a)
shuffle xs = map fst <<< Array.sortWith snd <$> traverse (\x -> Tuple x <$> Random.random) xs

outputHtml :: forall a cs m. GM.Output -> HH.ComponentHTML a cs m
outputHtml (SimpleOutput s) = HH.span_ [HH.text s]
outputHtml (ComplexOutput _ ) = HH.span_ [HH.text "todo"]

gameToRound :: forall cs m. GM.Game -> Effect (Round cs m)
gameToRound (GM.Game g) = do
  let correctAnswer = {answerText: g.gameSolution, answerCorrect: true, answerClicked: false}
      incorrectAnswers =  (\name -> {answerText: name, answerCorrect: false, answerClicked: false}) <$> g.gameOtherAnswers
  shuffledAnswers <- shuffle (Array.cons correctAnswer incorrectAnswers)
  pure { 
    quizLine: HH.span_ [
      HH.text "[",
      HH.span_ (Array.intersperse (HH.text ", ") (HH.text <$> g.gameInputs)), 
      HH.text "]",
      HH.text "==",
      (outputHtml g.gameOutput)
    ], 
    answers: shuffledAnswers
  } 

buildInitialState :: forall cs m. GM.Games -> Effect (State cs m)
buildInitialState (GM.Games gs) = do 
  rounds <- traverse gameToRound gs
  let c = unsafePartial $ fromJust $ Array.uncons rounds
  pure  {
    previousRounds: [],
    currentRound: Just c.head,
    upcomingRounds: c.tail
  }


data Action
  = Choose GM.MonoidName | AdvanceRound 

component :: forall q i o m. GM.Games -> Effect(H.Component q i o m)
component gs =do
  s <- buildInitialState gs
  pure $ H.mkComponent
    { initialState: \_ -> s
    , render: render 
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

renderButton :: forall cs m. Answer -> HH.ComponentHTML Action cs m
renderButton ans = 
    let GM.MonoidName name = ans.answerText
    in HH.button
        [HE.onClick \_ -> Choose ans.answerText]
        [ HH.text name ]


render :: forall cs m.  State cs m -> H.ComponentHTML Action cs m
render  state =
  case state.currentRound of
    Nothing -> HH.div_ [HH.text "Game Over"]
    Just r -> 
      HH.div_
        [ r.quizLine, 
          HH.p_ $ renderButton <$> r.answers
        ]

handleAction :: forall cs o m. Action → H.HalogenM (State cs m) Action cs o m Unit
handleAction = case _ of
  AdvanceRound -> H.modify_ \st -> st 
  Choose _ -> pure unit
