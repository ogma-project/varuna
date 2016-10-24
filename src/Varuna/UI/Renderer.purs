module Varuna.UI.Renderer where

import Prelude

import Control.Monad.Aff      (Aff, attempt)
import Data.Either            (Either(..))
import Data.Maybe             (Maybe(..))
-- halogen imports
import Halogen                (action, fromAff, HalogenEffects, ComponentDSL, ComponentHTML, Component, modify, gets)
import Halogen.HTML.Core      (ClassName)
import Halogen.Component      (lifecycleComponent)
import Halogen.HTML.Indexed   (div) as H
import Halogen.HTML.Properties.Indexed (classes) as P
-- varuna imports
import Varuna.Ogmarkup        (Language, VDOM, conf)
import Varuna.Component.Utils (raise) -- to be moved
import Text.Ogmarkup          (ogmarkup)
import Varuna.UI.HTML.Loading (loading)

-- renderer data
type PreviewDOM = VDOM Query

data Query a = Ask Language String a
             | Render Language String a
             | Display PreviewDOM a
             | Check a

data Queue = Free
           | Busy
           | Pending Language String
previewing :: Queue -> Boolean
previewing Free = false
previewing _ = true

data State = State { preview  :: PreviewDOM
                   , queue    :: Queue
                   }

getPreview :: State -> PreviewDOM
getPreview (State st) = st.preview

setPreview :: PreviewDOM -> State -> State
setPreview vdom (State st) = State st { preview = vdom }

getQueue :: State -> Queue
getQueue (State st) = st.queue

setQueue :: Queue -> State -> State
setQueue q (State st) = State st { queue = q }

-- component types
type RendererEffects eff = HalogenEffects (eff)
type RendererAff eff = Aff (RendererEffects eff)

-- render function
type RendererHTML = ComponentHTML Query

render :: Array ClassName
       -> State
       -> RendererHTML
render clss (State st) = H.div [ P.classes clss
                               ]
                               $ loading (previewing st.queue) st.preview

-- eval function
type RendererDSL eff = ComponentDSL State Query (RendererAff eff)

eval :: forall eff
      . Query
     ~> RendererDSL eff

eval (Ask lang txt next) = do
  queue <- gets getQueue
  case queue of Free      -> do modify $ setQueue Busy
                                raise <<< action $ Render lang txt
                _         -> modify <<< setQueue $ Pending lang txt
  pure next

eval (Render l txt next) = do
  at <- fromAff <<< attempt $ renderOgmarkup txt l
  case at of Right vdom -> do raise <<< action $ Display vdom
                              raise $ action  Check
             Left err   -> pure unit -- todo: dealing with errors
  pure next
    where
      renderOgmarkup :: String
                     -> Language
                     -> RendererAff eff PreviewDOM
      renderOgmarkup txt l = ogmarkup txt (conf l)

eval (Display vdom next) = do
  modify $ setPreview vdom
  pure next

eval (Check next) = do
  queue <- gets getQueue
  case queue of Pending lang txt -> do modify $ setQueue Busy
                                       raise <<< action $ Render lang txt
                _                -> modify $ setQueue Free
  pure next

-- component
type Renderer eff = Component State Query (RendererAff eff)

init :: State
init = State { preview: []
             , queue:   Free
             }

renderer :: forall eff
          . Array ClassName
         -> Renderer eff
renderer clss = lifecycleComponent { render: render clss
                                   , eval: eval
                                   , initializer: Nothing
                                   , finalizer: Nothing
                                   }
