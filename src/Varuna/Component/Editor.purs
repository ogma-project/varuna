module Varuna.Component.Editor where

-- stdlib import
import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Nothing))
-- halogen import
import Halogen (HalogenEffects, Component, ComponentHTML, ComponentDSL, get, set)
import Halogen.Component (lifecycleComponent)
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Indexed (textarea, div_, text) as H
import Halogen.HTML.Events.Indexed (onValueInput, input) as E
import Halogen.HTML.Properties.Indexed (value) as P
-- ogmarkup import
import Text.Ogmarkup (ogmarkup)
-- varuna import
import Varuna.Ogmarkup (conf, en)

type EditorEffects eff = HalogenEffects eff

data State = State { ogmarkup :: String
                   }

initState :: State
initState = State { ogmarkup: ""
                  }

data Query a = UpdateText String a
             | GetOgmarkup (String -> a)

render :: State
       -> ComponentHTML Query
render (State st) = H.div_ [ H.textarea [ E.onValueInput (E.input UpdateText)
                                        , P.value st.ogmarkup
                                        ]
                           , H.div_ (ogmarkup st.ogmarkup conf)
                           ]

eval :: forall eff
      . Query
     ~> ComponentDSL State Query (Aff (EditorEffects eff))
eval (UpdateText txt next) = do
  State st <- get
  let st' = st { ogmarkup = txt }
  set <<< State $ st'
  pure next
eval (GetOgmarkup continue) = do
  State st <- get
  pure $ continue st.ogmarkup

editor :: forall eff
        . Component State Query (Aff (EditorEffects eff))
editor = lifecycleComponent { render: render
                            , eval: eval
                            , initializer: Nothing
                            , finalizer: Nothing
                            }
