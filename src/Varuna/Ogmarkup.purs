module Varuna.Ogmarkup where

import Prelude (($), (<<<), class Eq, Void, Unit)
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

type VDOM f = Array (HTML Void (f Unit))

en :: forall f
     . Typography (VDOM f)
en = englishTypo mkO

fr :: forall f
    . Typography (VDOM f)
fr = frenchTypo mkO

data Language = Fr | En

derive instance eqLang :: Eq Language

lang2typo :: forall f
           . Language
          -> Typography (VDOM f)
lang2typo Fr = fr
lang2typo En = en

conf :: forall f
      . Language
     -> GenConf (VDOM f)
conf l = GC gc
  where
    gc = { typography:         lang2typo l
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

printHtmlSpace :: forall f. Space -> VDOM f
printHtmlSpace None = mkO ""
printHtmlSpace Nbsp = mkO $ S.singleton nbsp
printHtmlSpace Normal = mkO " "


docT :: forall f. Template (VDOM f)
docT = A.singleton <<< (H.article [])

errorT :: forall f. Template (VDOM f)
errorT = A.singleton <<< (H.span [])

storyT :: forall f. Template (VDOM f)
storyT = A.singleton <<< (H.div [])

asideT :: forall f. Maybe String -> Template (VDOM f)
asideT _ = A.singleton <<< (H.blockquote [])

paraT :: forall f. Template (VDOM f)
paraT = A.singleton <<< (H.p [])

tellT :: forall f. Template (VDOM f)
tellT = A.singleton <<< (H.span [])

dialogueT :: forall f. String -> Template (VDOM f)
dialogueT _ = A.singleton <<< (H.span [])

thoughtT :: forall f. String -> Template (VDOM f)
thoughtT _ = A.singleton <<< (H.span [])

replyT :: forall f. Template (VDOM f)
replyT = A.singleton <<< (H.span [])

between :: forall f. VDOM f
between = A.singleton $ H.br []

emphT :: forall f. Template (VDOM f)
emphT = A.singleton <<< (H.em [])

strongT :: forall f. Template (VDOM f)
strongT = A.singleton <<< (H.strong [])

author :: Maybe String -> String
author _ = "notMe"

mkO :: forall f
     . String
    -> VDOM f
mkO = A.singleton <<< H.text

