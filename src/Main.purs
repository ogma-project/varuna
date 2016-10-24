module Main where

import Prelude
import Control.Monad.Eff (Eff)

import Halogen (runUI)
import Halogen.Util (awaitBody, runHalogenAff)

import Varuna.UI.Editor (EditorEffects, editor, init)

main :: Eff (EditorEffects ()) Unit
main = do
  runHalogenAff $ do
    body <- awaitBody
    void $ runUI editor init body
