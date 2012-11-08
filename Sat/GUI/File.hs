module Sat.GUI.File where

import Lens.Family

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)

import Control.Monad.Trans.RWS

import Graphics.Rendering.Cairo.SVG

import Sat.GUI.Board
import Sat.GUI.GState

import Sat.VisualModel (visualToModel)
import Sat.VisualModels.FiguresBoard(Board,boardDefault)

createNewBoardFromLoad :: Board -> GuiMonad ()
createNewBoardFromLoad board = ask >>= \content -> do
    let model    = visualToModel board
        iconEdit = content ^. gSatIconEditBoard
        da       = content ^. gSatDrawArea 
    
    updateGState ((<~) gSatBoard board)
    updateGState ((<~) (gSatPieceToAdd . eaAvails) [])
    updateGState ((<~) (gSatPieceToAdd . eaMaxId) 0)
    updateGState ((<~) gSatModel model)
    
    io $ widgetHideAll iconEdit
    
    -- Esto es una manera de disparar un evento de Expose para re-dibujar.
    (drawWidth, drawHeight) <- io $ widgetGetSize da
    io $ widgetSetSizeRequest da drawWidth drawHeight
    -- Estaría bueno saber si se puede hacer de otra manera.
    
    return ()

-- | Crea un nuevo archivo en blanco.
createNewBoard :: GuiMonad ()
createNewBoard = createNewBoardFromLoad boardDefault
