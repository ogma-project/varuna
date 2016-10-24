module Varuna.UI.Editor where

import Prelude
import Control.Monad.Aff               (Aff)
import Data.Functor.Coproduct          (Coproduct)
import Data.Maybe                      (Maybe(..))
-- halogen imports
import Halogen                         (action, Component, query, ChildF, ParentDSL, ParentHTML, ParentState, parentComponent, parentState, gets, modify)
import Halogen.HTML.Core               (HTML, className)
import Halogen.HTML.Indexed            (slot, textarea, div, div_, text, input, h1_) as H
import Halogen.HTML.Events.Indexed     (onValueInput, input)
import Halogen.HTML.Properties.Indexed (checked, InputType(..), inputType, classes) as P
import Halogen.HTML.Events.Indexed     (onClick, input_) as E
import Halogen.Themes.Bootstrap3       as B
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
  H.div [ P.classes [ className "varuna-editor"
                    , className "fill"
                    ]
        ]
        [ menu
        , body
        ]
  where
    body = H.div [ P.classes [ B.container
                             , className "fill"
                             ]
                 ]
                 [ H.div [ P.classes [ B.row
                                     , className "editor-head"
                                     ]
                         ]
                         [ H.div [ P.classes [ B.colLg6 ] ]
                                 [ H.text "Markup" ]
                         , H.div [ P.classes [ B.colLg6 ] ]
                                 [ H.text "Preview" ]
                         ]
                 , H.div [ P.classes [ B.row
                                     , className "editor-main"
                                     ]
                         ]
                         [ editing
                         , renderer RendererSlot
                         ]
                 ]

    menu :: forall p
          . HTML p (InnerQuery Unit)
    menu = H.div [ P.classes [ className "varuna-editor-menu" ]
                 ]
                 [ H.h1_ [ H.text "Typography"
                         ]
                 , langSelection st.language
                 ]

    renderer slot = H.slot slot \_ -> { component:    Ren.renderer [ B.colLg6, className "varuna-renderer",  className "fill" ]
                                      , initialState: Ren.init
                                      }

    editing :: forall p
             . HTML p (InnerQuery Unit)
    editing = H.textarea [ onValueInput $ input Editing
                         , P.classes [ B.colLg6
                                     , className "fill"
                                     , className "varuna-textarea"
                                     ]
                         ]

    langSelection :: forall p
                   . Language
                  -> HTML p (InnerQuery Unit)
    langSelection current =
      H.div [
            ]
            [ langSelector current Fr
            , langSelector current En
            ]

    langSelector :: forall p
                  . Language
                 -> Language
                 -> HTML p (InnerQuery Unit)
    langSelector current option =
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
