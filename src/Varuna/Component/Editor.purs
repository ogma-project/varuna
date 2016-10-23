module Varuna.Component.Editor where

-- stdlib import
import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, later', attempt)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, AVar)
-- halogen import
import Halogen (HalogenEffects, fromAff, fromEff, Component, ComponentHTML, ComponentDSL, modify, set, get)
import Halogen.Component (lifecycleComponent)
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events.Indexed (onValueInput, onClick, input_, input) as E
import Halogen.HTML.Indexed (textarea, div_, text, form, input) as H
import Halogen.HTML.Properties.Indexed (value, checked, InputType(..), inputType) as P
import Halogen.Query (action)
-- ogmarkup import
import Text.Ogmarkup (ogmarkup)
-- varuna import
import Varuna.Component.Utils (raise)
import Varuna.Ogmarkup (conf, Language(..), VDOM)

type EditorEffects eff = HalogenEffects (console :: CONSOLE | eff)

type EditorDSL eff = ComponentDSL State Query (Aff (EditorEffects eff))
type EditorComponent eff = Component State Query (Aff (EditorEffects eff))
type EditorHTML = ComponentHTML Query

type PreviewDOM = VDOM Query

data PreviewState = Free
                  | Busy
                  | Pending String

showPS :: PreviewState -> String
showPS Free = "Done."
showPS Busy = "Rendering"
showPS (Pending txt) = "Rendering. Queue: " `append` txt

data State = State { ogmarkup :: String
                   , language :: Language
                   , preview  :: PreviewDOM
                   , queue   :: PreviewState
                   }

setOgmarkup :: String
            -> State
            -> State
setOgmarkup txt (State st) = State $ st { ogmarkup = txt }

setPreview :: PreviewDOM
            -> State
            -> State
setPreview vdom (State st) = State $ st { preview = vdom }

setLanguage :: Language
            -> State
            -> State
setLanguage l (State st) = State $ st { language = l }

initState :: State
initState = State { ogmarkup: ""
                  , language: En
                  , preview:  []
                  , queue:    Free
                  }

data Query a = AskPreview String a
             | CheckPreview a
             | RenderPreview String a
             | UpdatePreview PreviewDOM a
             | GetOgmarkup (String -> a)
             | SetLang Language a

render :: State
       -> EditorHTML
render (State st) = H.div_  [H.textarea [ E.onValueInput (E.input AskPreview)
                                        ]
                           , H.text $ showPS st.queue
                           , H.form []
                                    [ renderFrench st.language
                                    , renderEnglish st.language
                                    ]
                           , H.div_ $ st.preview
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
     ~> EditorDSL eff
eval (SetLang l next) = do
  modify $ setLanguage l
  State st <- get
  raise <<< action $ RenderPreview st.ogmarkup
  pure next
eval (GetOgmarkup continue) = do
  State st <- get
  pure $ continue st.ogmarkup
eval (AskPreview txt next) = do
  modify $ setOgmarkup txt
  State st <- get
  case st.queue of Free -> do set $ State st { queue = Busy }
                              raise <<< action $ RenderPreview txt
                   Busy -> do set $ State st { queue = Pending txt }
                   Pending _ -> set $ State st { queue = Pending txt }
  pure next
eval (CheckPreview next) = do
  State st <- get
  case st.queue of Pending txt -> do set $ State st { queue = Busy }
                                     fromEff $ log "pending preview has been found"
                                     raise <<< action $ RenderPreview txt
                   _           -> set $ State st { queue = Free }
  pure next
eval (RenderPreview txt next) = do
  State st <- get
  fromEff $ log "star render preview"
  at <- fromAff <<< attempt $ renderOgmarkup st.ogmarkup st.language
  case at of Right vdom -> do fromEff $ log "end render preview"
                              raise $ action (UpdatePreview vdom)
                              modify $ setPreview vdom
             Left err -> fromEff $ log (show err)
  pure next
eval (UpdatePreview vdom next) = do
  modify $ setPreview vdom
  raise $ action CheckPreview
  pure next

renderOgmarkup :: forall eff. String -> Language -> Aff (EditorEffects eff) PreviewDOM
renderOgmarkup txt l = ogmarkup txt (conf l)

editor :: forall eff
        . EditorComponent eff
editor = lifecycleComponent { render: render
                            , eval: eval
                            , initializer: Nothing
                            , finalizer: Nothing
                            }
