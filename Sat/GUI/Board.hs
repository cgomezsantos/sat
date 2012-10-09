-- | Renderiza el board para la interfaz en base a un archivo SVG.
module Sat.GUI.Board where

import Control.Monad 
import Control.Monad.Trans.RWS (ask,evalRWST,get)

import Lens.Family

import Data.Maybe
import qualified Data.Map as M

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)
import Graphics.UI.Gtk.Gdk.Events

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Sat.GUI.Piece
import Sat.GUI.GState

hSpacing :: Double
hSpacing = 20

-- | Función principal para el render del board.
renderBoard :: SVG -> Maybe PiecesToDraw -> GuiMonad ()
renderBoard board mptd = ask >>= \content -> io $ do
    let da = content ^. gSatDrawArea
    da `onExpose` \Expose { eventRegion = exposeRegion } -> do
        drawWindow              <- widgetGetDrawWindow da
        (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da

        renderWithDrawable drawWindow $ do
            let (boardWidth, boardHeight) = mapPair fromIntegral $ svgGetSize board
                sideSize = min drawWidth drawHeight - hSpacing
                xoffset  = (drawWidth - sideSize) / 2
                yoffset  = (drawHeight - sideSize) / 2
            region exposeRegion
            
            translate xoffset yoffset
            scale (sideSize / boardWidth) (sideSize / boardHeight)
            
            svgRender board
            maybe (return ()) renderPieces mptd
        return False
    return ()

configDrawPieceInBoard :: SVG -> GuiMonad ()
configDrawPieceInBoard b = ask >>= \content -> get >>= \rs -> getGState >>= \st ->
    io $ do
    let da = content ^. gSatDrawArea
    da `onButtonPress` \Button { eventButton = button
                               , eventClick = click
                               , eventX = x
                               , eventY = y
                               } -> do
        case (button,click) of
            (LeftButton,SingleClick) -> do
                (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da
                let sideSize   = min drawWidth drawHeight - hSpacing
                    squareSize = sideSize / 8
                    xoffset    = (drawWidth - sideSize) / 2
                    yoffset    = (drawHeight - sideSize) / 2
                when (x >= xoffset && x < xoffset + sideSize && y >= yoffset && y < yoffset + sideSize) $ do
                    let colx = floor ((x - xoffset) / squareSize)
                        rowy = floor ((y - yoffset) / squareSize)
                    putStrLn $ "Click" ++ "(" ++ show colx ++ "," ++ show rowy ++ ")"
                    evalRWST (do
                              addPieceToBoard colx rowy
                              st <- getGState
                              let pib = st ^. gSatPiecesInBoard
                              renderBoard b (Just pib)) content rs
                    widgetQueueDraw da
                return True
            (RightButton,SingleClick) -> do
                putStrLn "Esto debería borrar :P"
                return True
            _ -> return False
    return ()