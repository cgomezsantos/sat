module Sat.GUI.Piece where

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Control.Monad
import Control.Monad.Trans.RWS (ask)

import qualified Data.Map as M (insert,toList)

import Lens.Family

import Sat.GUI.GState

addPieceToBoard :: Int -> Int -> GuiMonad ()
addPieceToBoard x y = getGState >>= \st -> do
        let figToAdd    = st ^. (gSatPieceToAdd . paFig)
            predsForFig = st ^. (gSatPieceToAdd . paPreds)
        
        case figToAdd of
            Nothing -> return ()
            Just fig -> do
                let fSVG = fig ^. fiSVG
                updateGState (\s -> (<~) gSatPiecesInBoard 
                                         (M.insert 1 (renderPiece fSVG) (s ^. gSatPiecesInBoard)) s)
    where
        renderPiece :: SVG -> Render Bool
        renderPiece svg = svgRender svg
        
renderPieces :: PiecesToDraw -> Render ()
renderPieces pib = forM_ (M.toList pib) snd
