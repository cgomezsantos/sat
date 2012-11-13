module Sat.GUI.SVGBoardAlt (boardMain,boardMod) where

import Text.Blaze (toValue)
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Blaze.Svg11 ((!), mkPath, l, m, translate, scale)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import qualified Data.Map as M
import Data.Monoid(mappend)
import qualified Data.Colour.Names as Names
import Data.Colour(Colour,withOpacity,colourConvert)


import Sat.GUI.SVG
import Sat.Signatures.Figures


suitcase :: S.Svg
suitcase = 
        path (unlines [
          "m  40,32 0,160 -16,0"
         , "a  24,24 0 0 1 -24,-24 l 0,-112"
         , " a  24,24 0 0 1  24,-24 z"
         ])
         `mappend`
         S.rect ! A.x (intValue 48) 
                ! A.y (intValue 32)
                ! A.width (intValue 144)
                ! A.height (intValue 160)
         `mappend`
         path (unlines [
                   "m 200,32 16,0"
                  , "a  24,24 0 0 1  24,24  l 0,112"
                  , "a  24,24 0 0 1 -24,24  l -16,0 z"
                  ])
         `mappend`
         path (unlines [
                    "m  84,30 0,-12"
                  , " a  18,18 0 0 1 18,-18 l 36,0"
                  , "a  18,18 0 0 1 18,18  l 0,12 -12,0 0,-12"
                  , "a   6,6  0 0 0 -6,-6  l -36,0"
                  , "a   6,6  0 0 0 -6,6   l 0,12 z"
                  ]) ! A.fill (colorValue Names.red)


monkey = suitcase ! A.transform (toValue "scale(.5)")
fish = suitcase ! A.transform (toValue "scale (.5) rotate(-45) translate(-20)")
bird = suitcase ! A.transform (toValue "scale (.5) rotate(45) translate(-20 -40)")

redA   = colorAlpha Names.red        220
blueA  = colorAlpha Names.blue       227
greenA = colorAlpha Names.lightgreen 230

boardMain :: DrawMain
boardMain = M.fromList [ 
              (triangulo, suitcase)
            , (cuadrado,  fish)
            , (circulo,   bird)
            ]

-- -- Este mapa asocia cada predicado de la signatura con una función
-- -- que agrega atributos SVG para pintar de alguna forma ese predicado.
-- -- Si se cambia la signatura, debe modificarse este mapa.
boardMod :: M.Map Predicate (S.Svg -> S.Svg)
boardMod = M.fromList [
             (rojo,  redA)
           , (azul,  blueA)
           , (verde, greenA)
           , (grande,  big)
           , (mediano, normal)
           , (chico,   small)
           ]
