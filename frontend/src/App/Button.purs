module App.Button where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = { count :: Int }

data Action
  = Increment

component :: forall q i o m. String -> H.Component q i o m
component text =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render: render text
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. String -> State -> H.ComponentHTML Action cs m
render text state =
  HH.div_
    [ HH.text text
      
      , 
      HH.p_
        [ HH.text $ "You clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
