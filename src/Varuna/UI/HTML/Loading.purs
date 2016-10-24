module Varuna.UI.HTML.Loading where

import Data.Array                      ((:))
-- halogen imports
import Halogen.HTML.Core               (className, HTML)
import Halogen.HTML.Indexed            (div) as H
import Halogen.HTML.Properties.Indexed (classes) as P

loading :: forall p i
         . Boolean
        -> Array (HTML p i)
        -> Array (HTML p i)
loading true content = (H.div [ P.classes [ className "loader" ] ]
                              []):content
loading false content = (H.div [ P.classes [ className "loaded" ] ]
                               []):content
