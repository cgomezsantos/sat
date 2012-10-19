module Sat.GUI.SVG where

import qualified Data.Map as M
import Data.Maybe(fromJust)

import Graphics.Rendering.Cairo.SVG

import Sat.Signatures.Figures
import Sat.Core
import Sat.VisualModel
import Sat.VisualModels.FiguresBoard
import Sat.GUI.GState

data SVGInfo = SVGInfo { figurexml :: String
                       , colorxml  :: String
                       , sizexml   :: String
                       }
   

trsvg :: String
trsvg = "<polygon points=\"0,100 100,100 50,0\" stroke=\"black\" stroke-width=\"2\"/>"

cuadsvg :: String
cuadsvg = "<rect width=\"100\" height=\"100\" stroke=\"black\" stroke-width=\"2\"/>"

circsvg :: String
circsvg = "<circle cx=\"50\" cy=\"50\" r=\"50\" stroke=\"black\" stroke-width=\"2\"/>"

colorsvg :: String -> String
colorsvg c = "fill=\""++c++"\""

redsvg :: String
redsvg = colorsvg "red"

bluesvg :: String
bluesvg = colorsvg "blue"

greensvg :: String
greensvg = colorsvg "green"

sizesvg :: String -> String -> String
sizesvg tr sc = "transform=\"translate"++tr++" scale"++sc++"\""

bigsvg :: String
bigsvg = sizesvg "(25 25)" "(1.5 1.5)"

normalsizesvg :: String
normalsizesvg = sizesvg "(50 50)" "(1 1)"

smallsvg :: String
smallsvg = sizesvg "(75 75)" "(0.5 0.5)"


genFunFigureSVG :: Predicate -> String -> SVGInfo -> SVGInfo
genFunFigureSVG p xml s= 
    if figurexml s/= "" then error ("El predicado "++pname p++
                        " debe ser el primero en definirse para crear una figura")
                        else s { figurexml = xml }

-- Este mapa asocia cada predicado de la signatura con una función
-- que agrega atributos SVG para pintar de alguna forma ese predicado.
-- Si se cambia la signatura, debe modificarse este mapa.
guiPredicates :: M.Map Predicate (SVGInfo -> SVGInfo)
guiPredicates = 
  M.fromList [ (triangulo,genFunFigureSVG triangulo trsvg)
             , (cuadrado,genFunFigureSVG cuadrado cuadsvg)
             , (circulo,genFunFigureSVG circulo circsvg)
             , (rojo,\si -> si { colorxml = redsvg })
             , (azul,\si -> si { colorxml = bluesvg })
             , (verde,\si -> si { colorxml = greensvg })
             , (grande,\si -> si { sizexml = bigsvg })
             , (mediano,\si -> si { sizexml = normalsizesvg })
             , (chico,\si -> si { sizexml = smallsvg })
             ]
    

generateSVGFromEB :: ElemBoard -> IO SVG
generateSVGFromEB = generateSVG . interpPreds
    
generateSVG :: [Predicate]-> IO SVG
generateSVG ps = do
    let svginfo = foldl (\si p -> (fromJust $ M.lookup p guiPredicates) si) svginit ps
    svgNewFromString $ generateSVGString svginfo


    where svginit = SVGInfo { figurexml = ""
                             , colorxml = ""
                             , sizexml = normalsizesvg
                             }
          generateSVGString :: SVGInfo -> String
          generateSVGString si = 
            if figurexml si =="" then error "No puede haber un elemento sin figura"
                                 else
                "<svg width= \"200.00\" height= \"200.00\"> <g "++
                sizexml si ++ " "++ colorxml si ++ ">" ++
                figurexml si ++ "</g></svg>"

generateSVGS :: [Predicate] -> IO String
generateSVGS preds = do
    let svginfo = foldl (\si p -> (fromJust $ M.lookup p guiPredicates) si) svginit preds
    return $ generateSVGString svginfo


    where svginit = SVGInfo { figurexml = ""
                             , colorxml = ""
                             , sizexml = normalsizesvg
                             }
          generateSVGString :: SVGInfo -> String
          generateSVGString si = 
            if figurexml si =="" then error "No puede haber un elemento sin figura"
                                 else
                "<svg width= \"200.00\" height= \"200.00\"> <g "++
                sizexml si ++ " "++ colorxml si ++ ">" ++
                figurexml si ++ "</g></svg>"


