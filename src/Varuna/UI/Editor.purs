module Varuna.UI.Editor where

import Prelude
import Control.Monad.Aff               (Aff)
import Data.Functor.Coproduct          (Coproduct)
import Data.Maybe                      (Maybe(..))
-- halogen imports
import Halogen                         (action, Component, query, ChildF, ParentDSL, ParentHTML, ParentState, parentComponent, parentState, gets, modify)
import Halogen.HTML.Core               (HTML)
import Halogen.HTML.Indexed            (slot, textarea, div_, text, input) as H
import Halogen.HTML.Events.Indexed     (onValueInput, input)
import Halogen.HTML.Properties.Indexed (checked, InputType(..), inputType) as P
import Halogen.HTML.Events.Indexed     (onClick, input_) as E
-- varuna imports
import Varuna.UI.Renderer              as Ren
import Varuna.Ogmarkup                 (Language(..))

-- component
type Editor eff = Component (State eff) Query (EditorAff eff)

editor :: forall eff
        . Editor eff
editor = parentComponent { render, eval, peek: Just peek }

init :: forall eff
      . State eff
init = parentState $ InnerState { ogmarkup: ""
                                , language: En
                                }

-- render
type EditorHTML eff = ParentHTML Ren.State InnerQuery Ren.Query (EditorAff eff) EditorSlot

render :: forall eff
        . InnerState
       -> EditorHTML eff
render (InnerState st) =
  H.div_ [ H.textarea [ onValueInput $ input Editing ]
         , H.div_ [ renderLanguageSelector st.language Fr
                  , renderLanguageSelector st.language En
                  ]
         , H.slot RendererSlot \_ -> { component:    Ren.renderer
                                     , initialState: Ren.init
                                     }
         ]

renderLanguageSelector :: forall p
                        . Language
                       -> Language
                       -> HTML p (InnerQuery Unit)
renderLanguageSelector current option =
  H.div_ [ H.input [ P.checked $ current == option
                   , P.inputType P.InputRadio
                   , E.onClick <<< E.input_ $ SetLanguage option
                   ]
         , H.text $ case option of Fr -> "French"
                                   En -> "English"
         ]
-- query
type EditorDSL eff = ParentDSL InnerState Ren.State InnerQuery Ren.Query (EditorAff eff) EditorSlot

eval :: forall eff
      . InnerQuery
     ~> EditorDSL eff
eval (Editing txt next) = do
  modify $ setOgmarkup txt
  lang <- gets getLanguage
  query RendererSlot (action $ Ren.Ask lang txt)
  pure next
eval (SetLanguage lang next) = do
  modify $ setLanguage lang
  txt <- gets getOgmarkup
  query RendererSlot (action $ Ren.Ask lang txt)
  pure next

-- peek
peek :: forall eff a
      . ChildF EditorSlot Ren.Query a
     -> EditorDSL eff Unit
peek _ = pure unit

-- types
type EditorEffects eff = Ren.RendererEffects eff
type EditorAff eff = Aff (EditorEffects eff)

data EditorSlot = RendererSlot
derive instance eqEditorSlot :: Eq EditorSlot
derive instance ordEditorSlot :: Ord EditorSlot

data InnerState = InnerState { ogmarkup :: String
                             , language :: Language
                             }

getOgmarkup :: InnerState -> String
getOgmarkup (InnerState st) = st.ogmarkup

setOgmarkup :: String -> InnerState -> InnerState
setOgmarkup txt (InnerState st) = InnerState $ st { ogmarkup = txt }

getLanguage :: InnerState -> Language
getLanguage (InnerState st) = st.language

setLanguage :: Language -> InnerState -> InnerState
setLanguage l (InnerState st) = InnerState $ st { language = l }

data InnerQuery a = Editing String a
                  | SetLanguage Language a

type State eff = ParentState InnerState
                             Ren.State
                             InnerQuery
                             Ren.Query
                             (EditorAff eff)
                             EditorSlot

type Query = Coproduct InnerQuery
                       (ChildF EditorSlot Ren.Query)
