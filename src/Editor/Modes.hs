module Editor.Modes
       ( Mode (..)
       , createEditorOutput
       ) where

import qualified Data.Map as M
import Data.Maybe

import Commands
import Graphics.Vty (Key(..), Event(..), Modifier(..))

import Editor.Editor
import Editor.TextDisplay
import Editor.EditorTypes


{- | createEditorOutput creates a closure around an operation over editors
 - -}

createEditorOutput :: Editor a -> EditorOutput
createEditorOutput editor =
    let mode       = getMode $ getTextDisplay editor
        pic        = outputState mode editor
        next event = keyBindings mode event editor
    in EditorOutput pic False next
