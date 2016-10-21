module Varuna.Component.Editor where

-- stdlib import
import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Nothing))
-- halogen import
import Halogen (HalogenEffects, Component, ComponentHTML, ComponentDSL, modify, get)
import Halogen.Component (lifecycleComponent)
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Indexed (textarea, div_, text, form, input) as H
import Halogen.HTML.Events.Indexed (onValueInput, onClick, input_, input) as E
import Halogen.HTML.Properties.Indexed (value, checked, InputType(..), inputType) as P
-- ogmarkup import
import Text.Ogmarkup (ogmarkup)
-- varuna import
import Varuna.Ogmarkup (conf, Language(..))

type EditorEffects eff = HalogenEffects eff

data State = State { ogmarkup :: String
                   , language :: Language
                   }

setOgmarkup :: String
            -> State
            -> State
setOgmarkup txt (State st) = State $ st { ogmarkup = txt }

setLanguage :: Language
            -> State
            -> State
setLanguage l (State st) = State $ st { language = l }

initState :: State
initState = State { ogmarkup: ""
                  , language: En
                  }

data Query a = UpdateText String a
             | GetOgmarkup (String -> a)
             | SetLang Language a

render :: State
       -> ComponentHTML Query
render (State st) = H.div_ [ H.textarea [ E.onValueInput (E.input UpdateText)
                                        , P.value st.ogmarkup
                                        ]
                           , H.form []
                                    [ renderFrench st.language
                                    , renderEnglish st.language
                                    ]
                           , H.div_ $ ogmarkup st.ogmarkup (conf st.language)
                           ]
  where
    renderFrench l =
      H.div_ [ H.input [ P.checked $ l == Fr
                       , P.inputType P.InputRadio
                       , E.onClick <<< E.input_ $ SetLang Fr
                       ]
             , H.text "French"
             ]
    renderEnglish l =
      H.div_ [ H.input [ P.checked $ l == En
                       , P.inputType P.InputRadio
                       , E.onClick <<< E.input_ $ SetLang En
                       ]
             , H.text "English"
             ]


eval :: forall eff
      . Query
     ~> ComponentDSL State Query (Aff (EditorEffects eff))
eval (UpdateText txt next) = do
  modify $ setOgmarkup txt
  pure next
eval (SetLang l next) = do
  modify $ setLanguage l
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
