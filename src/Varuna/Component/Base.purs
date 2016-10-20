module Varuna.Component.Base where

import Prelude ((<<<), Unit)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)

log' :: forall eff
      . String
     -> Aff (console :: CONSOLE | eff) Unit
log' = liftEff <<< log
