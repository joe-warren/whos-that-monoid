module App.Quiz where

import Prelude

import Data.Array (any)
import Data.Array as Array
import Data.Foldable (class Foldable, sum, length)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random as Random
import Foreign.NullOrUndefined (undefined)
import GameModel (MonoidName, Output(..))
import GameModel as GM
import Halogen (ClassName)
import Halogen as H
import Halogen.HTML (output)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.HTML.Event.EventTypes (offline)


data RoundState = Win | Lose | InPlay

derive instance roundStateEq :: Eq RoundState

roundState :: forall cs m. Round cs m -> RoundState
roundState r = if any (\a -> a.answerClicked && a.answerCorrect) r.answers
                then Win
                else if any (_.answerClicked) r.answers
                      then Lose
                      else InPlay

type Answer = { answerText :: GM.MonoidName, 
      answerCorrect :: Boolean,
      answerClicked :: Boolean
    }

type Round cs m = {
    quizLine :: HH.ComponentHTML Action cs m, 
    answers :: Array Answer,
    roundImage :: Int
  }

type State cs m 
  = { previousRounds :: Array (Round cs m),
      currentRound :: Maybe (Round cs m), 
      upcomingRounds :: Array (Round cs m)
    }

allRounds :: forall cs m. State cs m -> List (Round cs m)
allRounds s =  List.fromFoldable s.previousRounds <> 
     List.fromFoldable s.currentRound <> 
     List.fromFoldable s.upcomingRounds

shuffle :: ∀ a. Array a -> Effect (Array a)
shuffle xs = map fst <<< Array.sortWith snd <$> traverse (\x -> Tuple x <$> Random.random) xs

outputHtml :: forall a cs m. GM.Output -> HH.ComponentHTML a cs m
outputHtml (SimpleOutput s) = HH.span_ [HH.text "==", HH.text s]
outputHtml (ComplexOutput (GM.ComplexOutputFields f) ) = HH.span_ [
  HH.text "<$>",
  HH.text "[",
  HH.span_ (Array.intersperse (HH.text ", ") (HH.text <$> f.complexOutputInputs)), 
  HH.text "]",
  HH.text " == ",
  HH.text "[",
  HH.span_ (Array.intersperse (HH.text ", ") (HH.text <$> f.complexOutputOutputs)), 
  HH.text "]"
]

classname = HP.class_ <<< HH.ClassName

gameToRound :: forall cs m. Tuple GM.Game Int -> Effect (Round cs m)
gameToRound (Tuple (GM.Game g) i) = do
  let correctAnswer = {answerText: g.gameSolution, answerCorrect: true, answerClicked: false}
      incorrectAnswers =  (\name -> {answerText: name, answerCorrect: false, answerClicked: false}) <$> g.gameOtherAnswers
  shuffledAnswers <- shuffle (Array.cons correctAnswer incorrectAnswers)
  pure { 
    quizLine: HH.span [classname "quizline"] [
      HH.text "ala ", 
      HH.text " ???? ",
      HH.text " foldMap ",
      HH.text "[",
      HH.span_ (Array.intersperse (HH.text ", ") (HH.text <$> g.gameInputs)), 
      HH.text "]",
      HH.text " ",
      (outputHtml g.gameOutput)
    ], 
    answers: shuffledAnswers,
    roundImage: i
  } 

buildInitialState :: forall cs m. GM.Games -> Effect (State cs m)
buildInitialState (GM.Games gs) = do 
  roundImages <- shuffle (Array.range 0 5)
  rounds <- traverse gameToRound (Array.zip gs roundImages)
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

renderButton :: forall cs m. RoundState -> Answer -> HH.ComponentHTML Action cs m
renderButton r ans = 
    let GM.MonoidName name = ans.answerText
        clazz = HH.ClassName $ case r of
          InPlay -> "undetermined"
          _ -> if ans.answerCorrect 
                  then "correct"
                  else if ans.answerClicked
                    then "incorrect"
                    else "undetermined"
        isEnabled = HP.disabled $ case r of
          InPlay -> false
          _ -> true
    in HH.button
        [HE.onClick \_ -> Choose ans.answerText, 
          HP.class_ clazz, 
          isEnabled
        ]
        [ HH.text name ]


count :: forall t a. Traversable t => (a -> Boolean) -> t a -> Int
count f xs = let f' v =  if f v then 1 else 0 
              in sum $ f' <$> xs


score :: forall cs m. State cs m -> Tuple Int Int
score s = Tuple (count (((==) Win) <<< roundState) s.previousRounds) (length s.previousRounds)



progressBar :: forall cs m. State cs m -> H.ComponentHTML Action cs m
progressBar s = let entity InPlay = HH.img [HP.src "static/imgs/active.gif"]
                    entity Win = HH.img [HP.src "static/imgs/success.gif"]
                    entity Lose = HH.img [HP.src "static/imgs/lose.gif"]
                    notPlayedEntity = HH.img [HP.src "static/imgs/inactive.gif"]
              in HH.p [HP.class_ (HH.ClassName "progress")] $ 
                  (entity <<< roundState <$> ( s.previousRounds <> Array.fromFoldable s.currentRound)) <>
                  (const notPlayedEntity <$> s.upcomingRounds)

imgUrl :: forall cs a. Round cs a -> String
imgUrl r = 
  let prefix = 
             case roundState r of 
                InPlay -> "-sill.svg"
                _ -> "-full.svg"
  in "static/imgs/" <> show r.roundImage <>  prefix

render :: forall cs m.  State cs m -> H.ComponentHTML Action cs m
render state =
  HH.div [classname "content"] $ case state.currentRound of
    Nothing -> [
    progressBar state,
    HH.text "Game Over",
    let Tuple s nRounds = score state in HH.p_ [
      HH.text (show s),
      HH.text "/",
      HH.text $ show nRounds
    ]
    ]
    Just r -> 
        [ 
          progressBar state,
          HH.p_ [
            HH.img [classname "bigGraphic", HP.src $ imgUrl r]
          ],
          HH.p_ [r.quizLine], 
          HH.p_ $ renderButton (roundState r) <$> r.answers,
          HH.p_ $ case roundState r of
                    InPlay -> []
                    _ ->  [HH.button
                            [HE.onClick \_ -> AdvanceRound]
                            [ HH.text "Next Question" ]
                      ]
        ]

  

doChoose :: forall cs m. MonoidName -> State cs m -> State cs m
doChoose name s = 
  let updateAnswer a = a { answerClicked = a.answerText == name }
      updateCurrentRound r = r { answers = (updateAnswer <$> r.answers) }
  in s { currentRound = updateCurrentRound <$> s.currentRound }

doAdvance :: forall cs m.  State cs m -> State cs m
doAdvance s = 
  let c = Array.uncons s.upcomingRounds
  in {
    previousRounds: (maybe identity (flip Array.snoc) s.currentRound) s.previousRounds,
    currentRound: _.head <$> c,
    upcomingRounds: maybe [] _.tail c
  }


handleAction :: forall cs o m. Action → H.HalogenM (State cs m) Action cs o m Unit
handleAction = case _ of
  AdvanceRound -> H.modify_ doAdvance
  Choose n -> H.modify_ (doChoose n)
