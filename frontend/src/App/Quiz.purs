module App.Quiz
  ( Action(..)
  , Answer
  , Round
  , RoundState(..)
  , State
  , allRounds
  , app
  , bold
  , buildInitialState
  , classname
  , code
  , component
  , count
  , doAdvance
  , doChoose
  , fun
  , gameToRound
  , handleAction
  , highlight
  , imgUrl
  , inp
  , inputs
  , list
  , newGameLink
  , nonEmptyList
  , op
  , out
  , outputHtml
  , progressBar
  , questionsRemain
  , render
  , renderButton
  , resultsText
  , roundState
  , score
  , sharingLink
  , shuffle
  , space
  )
  where

import Prelude

import Data.Array (any)
import Data.Array as Array
import Data.Foldable (class Foldable, sum, length)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern (..), Replacement (..))
import Effect (Effect)
import Effect.Random as Random
import Foreign.NullOrUndefined (undefined)
import GameModel (MonoidName, Output(..))
import GameModel as GM
import Halogen (ClassName)
import Halogen as H
import Halogen.HTML (a, output)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.ValidityState (valid)
import Web.URL as URL
import Web.URL.URLSearchParams as URLParams

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
  = { 
      pageUrl :: String,
      nextUrl :: String, 
      previousRounds :: Array (Round cs m),
      currentRound :: Maybe (Round cs m), 
      upcomingRounds :: Array (Round cs m)
    }

allRounds :: forall cs m. State cs m -> List (Round cs m)
allRounds s =  List.fromFoldable s.previousRounds <> 
     List.fromFoldable s.currentRound <> 
     List.fromFoldable s.upcomingRounds

shuffle :: ∀ a. Array a -> Effect (Array a)
shuffle xs = map fst <<< Array.sortWith snd <$> traverse (\x -> Tuple x <$> Random.random) xs

code :: forall a cs m. String -> String -> HH.ComponentHTML a cs m
code clazz content = HH.span [classname clazz] [HH.text content]


fun = code "function"

op = code "operator"

inp = code "input"

out = code "output"

app = code "applied"

bold = HH.span [classname "bold"] <<< pure

outputHtml prefix (SimpleOutput s) = HH.span_ [prefix, space, op "==", space,  bold $ out s]
outputHtml prefix (ComplexOutput (GM.ComplexOutputFields f) ) = HH.span_ [
    (case f.complexOutputApplication of
      Just app -> HH.span_ [fun app, space, op "(", prefix, op ")"]
      Nothing -> prefix 
    ) 
    ,
    space, op "<$>", space,
    bold $ list (app <$> f.complexOutputInputs), 
    HH.br_,
    op " == ",
    bold $ list (out <$> f.complexOutputOutputs)
  ]

space = HH.text " "

classname = HP.class_ <<< HH.ClassName

highlight :: forall a cs m. String -> HH.ComponentHTML a cs m
highlight = HH.text

list :: forall m cs a. Array (HH.ComponentHTML m cs a)-> HH.ComponentHTML m cs a
list a = HH.span_ [op "[", HH.span_ $ Array.intersperse (op ", ") a, op "]"]

nonEmptyList :: forall m cs a. Array (HH.ComponentHTML m cs a) -> HH.ComponentHTML m cs a
nonEmptyList a = case Array.uncons a of 
  Just v -> HH.span_ $ [op "((",  v.head, op ")", space, op ":|", space] <> [list v.tail, op ")"]
  _ -> list a

inputs ::forall cs m. GM.Game -> HH.ComponentHTML Action cs m
inputs (GM.Game g) = bold $ case g.gameAggregation of
                    "foldMap1" -> nonEmptyList $ inp <$> g.gameInputs
                    _ -> list $ inp <$> g.gameInputs

gameToRound :: forall cs m. Tuple GM.Game Int -> Effect (Round cs m)
gameToRound (Tuple (GM.Game g) i) = do
  let correctAnswer = {answerText: g.gameSolution, answerCorrect: true, answerClicked: false}
      incorrectAnswers =  (\name -> {answerText: name, answerCorrect: false, answerClicked: false}) <$> g.gameOtherAnswers
  shuffledAnswers <- shuffle (Array.cons correctAnswer incorrectAnswers)
  pure { 
    quizLine: HH.span [classname "quizline"] [
      let prefix = HH.span_ [fun "ala ", 
          HH.span [classname "tinyquestionmarks"] (Array.replicate 5 (HH.img [HP.src "static/imgs/question-mark.svg"])),
          space, fun g.gameAggregation, space,
          inputs (GM.Game g)]
       in 
      (outputHtml prefix g.gameOutput)
    ], 
    answers: shuffledAnswers,
    roundImage: i
  } 

buildInitialState :: forall cs m. String -> String -> GM.Games -> Effect (State cs m)
buildInitialState url nextUrl (GM.Games gs) = do 
  roundImages <- shuffle (Array.range 0 5)
  rounds <- traverse gameToRound (Array.zip gs roundImages)
  let c = unsafePartial $ fromJust $ Array.uncons rounds
  pure  {
    pageUrl: url,
    nextUrl: nextUrl,
    previousRounds: [],
    currentRound: Just c.head,
    upcomingRounds: c.tail
  }


data Action
  = Choose GM.MonoidName | AdvanceRound 

component :: forall q i o m. String -> String -> GM.Games -> Effect(H.Component q i o m)
component s n gs = do
  s <- buildInitialState s n gs
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
              in HH.div [HP.class_ (HH.ClassName "progress")] $ 
                  (entity <<< roundState <$> ( s.previousRounds <> Array.fromFoldable s.currentRound)) <>
                  (const notPlayedEntity <$> s.upcomingRounds)

imgUrl :: forall cs a. Round cs a -> String
imgUrl r = 
  let prefix = 
             case roundState r of 
                InPlay -> "-sill.svg"
                _ -> "-full.svg"
  in "static/imgs/" <> show r.roundImage <>  prefix

resultsText :: forall cs m. State cs m -> String
resultsText state = let Tuple nScore nRounds = score state
               in show nScore <> "/" <> show nRounds


questionsRemain :: forall cs m. State cs m -> Boolean
questionsRemain = not <<< Array.null <<<_.upcomingRounds


sharingLink :: forall cs m . State cs m -> HH.ComponentHTML Action cs m
sharingLink s = 
    let 
      text = "I scored " <> resultsText s <> " on \"Who's That Monoid\"\n" <> s.pageUrl <> "\nTry it out?\n" 
        <> "via @hungryjoe@functional.cafe\n"
        <> "#Haskell #WhosThatMonoid"
      plainUrl = URL.unsafeFromAbsolute "https://toot.kytta.dev/"
      -- toot.kytta.dev doesn't support + in URL parameters
      expandify = replaceAll (Pattern "+") (Replacement "%20")
      params = expandify <<< URLParams.toString <<<
          URLParams.append "text" text $
          URLParams.fromString ""
      sharingUrl = URL.toString $ URL.setSearch params plainUrl
    in HH.a [HP.href sharingUrl] [HH.text "Toot On Mastodon?"]

newGameLink :: forall cs m . State cs m -> HH.ComponentHTML Action cs m
newGameLink s = HH.a [HP.href s.nextUrl] [HH.text "Play Again?"]

render :: forall cs m.  State cs m -> H.ComponentHTML Action cs m
render state =
  HH.div [classname "content"] ([
   HH.div [classname "header"] [
     HH.img [HP.src "static/imgs/small-title.svg"] , 
     HH.a [HP.href "help.html", HP.target "_blank"] [HH.text "Help"]
     ]
  ] <> (case state.currentRound of
    Nothing -> [
        progressBar state,
        HH.div [classname "resultsPage"] [
          HH.h1_ [HH.text "Results"],
          HH.div [classname "resultsText"] [ HH.text (resultsText state) ],
          HH.p_ <<< Array.replicate (fst <<< score $ state) $ HH.img [HP.src "static/imgs/rosette.svg"] , 
          HH.p_ [
            newGameLink state,
            sharingLink state
          ]
        ]
    ]
    Just r -> 
        [ 
          progressBar state,
          HH.img [classname "bigGraphic", HP.src $ imgUrl r],
          HH.div [classname "quizSection"] [
            HH.p_ [r.quizLine], 
            HH.div [classname "buttons"] $ renderButton (roundState r) <$> r.answers,
            HH.div [classname "nextQuestion"] $ case roundState r of
                      InPlay -> [ ]
                      _ ->  [HH.button
                              [HE.onClick \_ -> AdvanceRound]
                              [ HH.text $ if questionsRemain state then "Next Question" else "See Results"]
                        ]
  ]]) <> [
          HH.div [classname "links"]
            [
              HH.a [HP.href "https://github.com/joe-warren/whos-that-monoid/"] [HH.img [HP.src "static/imgs/github.svg"]],
              HH.a [HP.href "https://@functional.cafe/@hungryjoe"] [HH.img [HP.src "static/imgs/mastodon.svg"]]
            ]
        ])

  

doChoose :: forall cs m. MonoidName -> State cs m -> State cs m
doChoose name s = 
  let updateAnswer a = a { answerClicked = a.answerText == name }
      updateCurrentRound r = r { answers = (updateAnswer <$> r.answers) }
  in s { currentRound = updateCurrentRound <$> s.currentRound }

doAdvance :: forall cs m.  State cs m -> State cs m
doAdvance s = 
  let c = Array.uncons s.upcomingRounds
  in s {
    previousRounds = (maybe identity (flip Array.snoc) s.currentRound) s.previousRounds,
    currentRound = _.head <$> c,
    upcomingRounds = maybe [] _.tail c
  }


handleAction :: forall cs o m. Action → H.HalogenM (State cs m) Action cs o m Unit
handleAction = case _ of
  AdvanceRound -> H.modify_ doAdvance
  Choose n -> H.modify_ (doChoose n)
