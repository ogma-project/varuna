module Varuna.Ogmarkup where

import Prelude (($), (<<<))
import Data.Maybe (Maybe(..))
import Data.Char (fromCharCode)
import Data.String (singleton) as S
import Data.Array (singleton) as A

import Text.Ogmarkup (Space(..), Typography(..), Template, frenchTypo,  englishTypo)
import Text.Ogmarkup.Private.Config (GenConf(..)) -- to be removed

import Halogen.HTML.Core (HTML)
import Halogen.HTML.Indexed (text) as H
import Halogen.HTML.Elements.Indexed (article, blockquote, br, div, em, p, span, strong) as H

nbsp :: Char
nbsp = fromCharCode 0x00a0

en :: forall p i
     . Typography (Array (HTML p i))
en = englishTypo mkO

fr :: forall p i
    . Typography (Array (HTML p i))
fr = frenchTypo mkO

conf :: forall p i
      . GenConf (Array (HTML p i))
conf = GC gc
  where
    gc = { typography:         en
         , documentTemplate:   docT
         , errorTemplate:      errorT
         , storyTemplate:      storyT
         , asideTemplate:      asideT
         , paragraphTemplate:  paraT
         , tellerTemplate:     tellT
         , dialogueTemplate:   dialogueT
         , thoughtTemplate:    thoughtT
         , replyTemplate:      replyT
         , betweenDialogue:    between
         , emphTemplate:       emphT
         , strongEmphTemplate: strongT
         , authorNormalize:    author
         , printSpace:         printHtmlSpace
         }

printHtmlSpace :: forall p i. Space -> Array (HTML p i)
printHtmlSpace None = mkO ""
printHtmlSpace Nbsp = mkO $ S.singleton nbsp
printHtmlSpace Normal = mkO " "


docT :: forall p i. Template (Array (HTML p i))
docT = A.singleton <<< (H.article [])

errorT :: forall p i. Template (Array (HTML p i))
errorT = A.singleton <<< (H.span [])

storyT :: forall p i. Template (Array (HTML p i))
storyT = A.singleton <<< (H.div [])

asideT :: forall p i. Maybe String -> Template (Array (HTML p i))
asideT _ = A.singleton <<< (H.blockquote [])

paraT :: forall p i. Template (Array (HTML p i))
paraT = A.singleton <<< (H.p [])

tellT :: forall p i. Template (Array (HTML p i))
tellT = A.singleton <<< (H.span [])

dialogueT :: forall p i. String -> Template (Array (HTML p i))
dialogueT _ = A.singleton <<< (H.span [])

thoughtT :: forall p i. String -> Template (Array (HTML p i))
thoughtT _ = A.singleton <<< (H.span [])

replyT :: forall p i. Template (Array (HTML p i))
replyT = A.singleton <<< (H.span [])

between :: forall p i. Array (HTML p i)
between = A.singleton $ H.br []

emphT :: forall p i. Template (Array (HTML p i))
emphT = A.singleton <<< (H.em [])

strongT :: forall p i. Template (Array (HTML p i))
strongT = A.singleton <<< (H.strong [])

author :: Maybe String -> String
author _ = "notMe"

mkO :: forall p i
     . String
    -> Array (HTML p i)
mkO = A.singleton <<< H.text

