module App.IntroScreen where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.HTML.Properties as HP
type State = String

type Action = Void

classname = HP.class_ <<< HH.ClassName

component :: forall q i o m. String -> H.Component q i o m
component gameId =
  H.mkComponent
    { initialState: \_ -> gameId
    , render: render 
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m.  State -> H.ComponentHTML Action cs m
render state =
  HH.div [classname "intro"]
    [ HH.p_ [ HH.img [HP.src "static/imgs/social.png"] ],
      HH.p_ [ HH.a [HP.href $ "?game=" <> state ] [HH.text "Play?"] ] 
    ]


handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = const (pure unit)
