module Varuna.Component.Editor where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Nothing))

import Halogen (HalogenEffects, Component, ComponentHTML, ComponentDSL, get, set)
import Halogen.Component (lifecycleComponent)
import Halogen.HTML.Indexed (textarea, div_, text) as H
import Halogen.HTML.Events.Indexed (onValueInput, input) as E
import Halogen.HTML.Properties.Indexed (value) as P

type EditorEffects eff = HalogenEffects eff

data State = State { ogmarkup :: String
                   , html     :: String
                   }

initState :: State
initState = State { ogmarkup: ""
                  , html: ""
                  }

data Query a = UpdateText String a
             | GetOgmarkup (String -> a)
             | GetHtml (String -> a)

render :: State
       -> ComponentHTML Query
render (State st) = H.div_ [ H.textarea [ E.onValueInput (E.input UpdateText)
                                        , P.value st.ogmarkup
                                        ]
                           , H.text (st.html)
                           ]

eval :: forall eff
      . Query
     ~> ComponentDSL State Query (Aff (EditorEffects eff))
eval (UpdateText txt next) = do
  State st <- get
  let st' = st { ogmarkup = txt }
  set <<< State $ st'
  let txt' = txt -- need ogmarkup call here
  set <<< State $ st' { html = txt' }
  pure next
eval (GetOgmarkup continue) = do
  State st <- get
  pure $ continue st.ogmarkup
eval (GetHtml continue) = do
  State st <- get
  pure $ continue st.html

editor :: forall eff
        . Component State Query (Aff (EditorEffects eff))
editor = lifecycleComponent { render: render
                            , eval: eval
                            , initializer: Nothing
                            , finalizer: Nothing
                            }
