-- | Renderiza el board para la interfaz en base a un archivo SVG.
module Sat.GUI.Board where

import Control.Monad 
import Control.Monad.Trans.RWS (ask,evalRWST,get)

import Lens.Family

import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)
import Graphics.UI.Gtk.Gdk.Events

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Sat.VisualModels.FiguresBoard

import Sat.GUI.SVG
import Sat.GUI.Piece
import Sat.GUI.GState
import Sat.GUI.FigureList

hSpacing :: Double
hSpacing = 20

-- | Función principal para el render del board.
renderBoard :: SVG -> GuiMonad ()
renderBoard svgboard = ask >>= \content -> getGState >>= \st -> io $ do
    let da = content ^. gSatDrawArea
        board = st ^. gSatBoard
    da `onExpose` \Expose { eventRegion = exposeRegion } -> do
        drawWindow              <- widgetGetDrawWindow da
        (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da

        renderWithDrawable drawWindow $ do
            let (boardWidth, boardHeight) = mapPair fromIntegral $ svgGetSize svgboard
                sideSize = min drawWidth drawHeight - hSpacing
                xoffset  = (drawWidth - sideSize) / 2
                yoffset  = (drawHeight - sideSize) / 2
            region exposeRegion
            
            translate xoffset yoffset
            
            save
            scale (sideSize / boardWidth) (sideSize / boardHeight)
            svgRender svgboard
            restore
            
            renderElems board sideSize
        return False
    return ()

renderElems :: Board -> Double -> Render ()
renderElems b sideSize =
  forM_ (elems b) $ \(Coord x y, e) -> do
      svgelem <- io $ generateSVGFromEB e
      let squareSize = sideSize / realToFrac (size b)
          (width, height) = mapPair fromIntegral (svgGetSize svgelem)
      
      save
      translate (squareSize * fromIntegral (toEnum x)) (squareSize * fromIntegral (toEnum y))
      scale (squareSize / width) (squareSize / height)
      svgRender svgelem
      restore
    
    
configDrawPieceInBoard :: SVG -> GuiMonad ()
configDrawPieceInBoard b = ask >>= \content -> get >>= \rs -> io $ do
    let da = content ^. gSatDrawArea
    da `onButtonPress` \Button { eventButton = button
                               , eventClick = click
                               , eventX = x
                               , eventY = y
                               } -> do
        (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da
        let sideSize   = min drawWidth drawHeight - hSpacing
            squareSize = sideSize / 8
            xoffset    = (drawWidth - sideSize) / 2
            yoffset    = (drawHeight - sideSize) / 2
        when (x >= xoffset && x < xoffset + sideSize && y >= yoffset && y < yoffset + sideSize) $ do
            let colx = floor ((x - xoffset) / squareSize)
                rowy = floor ((y - yoffset) / squareSize)
            case (button,click) of
                (LeftButton,SingleClick) -> do
                    evalRWST (addElemBoardAt colx rowy) content rs
                    widgetQueueDraw da
                (RightButton,SingleClick) -> do
                    evalRWST (deleteElemBoardAt colx rowy) content rs
                    widgetQueueDraw da
                _ -> return ()
        return True
    return ()
    where
        deleteElemBoardAt :: Int -> Int -> GuiMonad ()
        deleteElemBoardAt colx rowy = do
            st <- getGState
            let board = st ^. gSatBoard
                elemsB = elems board 
                
                avails = st ^. (gSatPieceToAdd . eaAvails)
                
                cords = Coord colx rowy
                elemToDelete = lookup cords elemsB
            
            when (isJust elemToDelete)
                   (updateGState ((<~) gSatBoard (board {elems = L.delete (cords,fromJust elemToDelete) elemsB})) >>
                    updateGState ((<~) (gSatPieceToAdd . eaAvails) ((uElemb $ fromJust elemToDelete) : avails))
                   )
            
            renderBoard b
        
        addElemBoardAt :: Int -> Int -> GuiMonad ()
        addElemBoardAt colx rowy = do
            st <- getGState
            let board  = st ^. gSatBoard
                
                preds  = st ^. (gSatPieceToAdd . eaPreds)
                avails = st ^. (gSatPieceToAdd . eaAvails)
                i      = st ^. (gSatPieceToAdd . eaMaxId)
                
                cords = Coord colx rowy
                (newElemBoard,avails') = 
                    if avails == []
                        then ((cords,ElemBoard (i + 1) preds),avails)
                        else ((cords,ElemBoard (head avails) preds),tail avails)
            
            updateGState ((<~) gSatBoard (addElem newElemBoard board))
            updateGState ((<~) (gSatPieceToAdd . eaMaxId) (i+1))
            updateGState ((<~) (gSatPieceToAdd . eaAvails) avails')

            renderBoard b
            where
                addElem :: (Coord,ElemBoard) -> Board -> Board
                addElem eb b = let elemsB = elems b in
                        case lookup (fst eb) elemsB of
                            Nothing -> (b {elems = eb : elemsB})
                            Just _ -> b
