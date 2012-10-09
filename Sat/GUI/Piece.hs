module Sat.GUI.Piece where

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Control.Monad
import Control.Monad.Trans.RWS (ask)

import qualified Data.Map as M (insert,toList,fromList)

import Lens.Family

import Sat.GUI.GState
import Sat.Signatures.Figures


-- addPieceToBoard :: Int -> Int -> GuiMonad ()
-- addPieceToBoard x y = getGState >>= \st -> do
--         let figToAdd    = st ^. (gSatPieceToAdd . paFig)
--             predsForFig = st ^. (gSatPieceToAdd . paPreds)
--         
--         case figToAdd of
--             Nothing -> return ()
--             Just fig -> do
--                 let fSVG = fig ^. fiSVG
--                 updateGState (\s -> (<~) gSatPiecesInBoard 
--                                          (M.insert 1 (renderPiece fSVG) (s ^. gSatPiecesInBoard)) s)
--     where
--         renderPiece :: SVG -> Render Bool
--         renderPiece svg = svgRender svg
--         
   
   
genFunFigureSVG :: Predicate -> String -> SVGInfo -> SVGInfo
genFunFigureSVG p xml s= 
    if figurexml s/= "" then error "El predicado "++pname p++
                        " debe ser el primero en definirse para crear una figura"
                        else s { figurexml = xml }

-- Este mapa asocia cada predicado de la signatura con una función
-- que agrega atributos SVG para pintar de alguna forma ese predicado.
-- Si se cambia la signatura, debe modificarse este mapa.
guiPredicates :: M.Map Predicate (SVGInfo -> SVGInfo)
guiPredicates = 
  M.fromList [ (triangulo,genFunFigureSVG triangulo trsvg)
	     , (cuadrado,genFunFigureSVG cuadrado cuadsvg)
	     , (circulo,genFunFigureSVG circulo circsvg)
             , (rojo,\si -> si { colorxml = redsvg }
             , (azul,\si -> si { colorxml = bluesvg }
             , (verde,\si -> si { colorxml = greensvg }
             , (grande,\si -> si { sizexml = bigsvg }
             , (chico,\si -> si { sizexml = smallsvg }
             ]
    

  




        
        
        
renderPieces :: PiecesToDraw -> Render ()
renderPieces pib = forM_ (M.toList pib) snd
