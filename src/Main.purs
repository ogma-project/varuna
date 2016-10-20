module Main where

import Prelude
import Control.Monad.Eff (Eff)

import Halogen (runUI)
import Halogen.Util (awaitBody, runHalogenAff)

import Varuna.Component.Editor (EditorEffects, editor, initState)

main :: Eff (EditorEffects ()) Unit
main = do
  runHalogenAff $ do
    body <- awaitBody
    void $ runUI editor initState body
