module Sat.GUI.SVGBoard (boardMain,boardMod) where

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import qualified Data.Map as M


import Sat.GUI.SVG
import Sat.Signatures.Figures


triangle,square,circle :: S.Svg
triangle = S.polygon ! A.points (mconcat $ map pairCommaValue dim)
                     ! blackStroke
    where dim :: [(Int,Int)]
          dim = [(0,100),(100,100), (50,0)]

square = S.rect ! A.width (intValue 100) 
                ! A.height (intValue 100)
                ! blackStroke

circle = S.circle ! A.r (intValue 50) 
                  ! A.cx (intValue 50)
                  ! A.cy (intValue 50)
                  ! blackStroke


boardMain :: DrawMain
boardMain = M.fromList [ 
              (triangulo, triangle)
            , (cuadrado,  square)
            , (circulo,   circle)
            ]

-- | Este mapa asocia cada predicado de la signatura con una función
-- que agrega atributos SVG para pintar de alguna forma ese predicado.
-- Si se cambia la signatura, debe modificarse este mapa.
boardMod :: M.Map Predicate (S.Svg -> S.Svg)
boardMod = M.fromList [
             (rojo,  red)
           , (azul,  blue)
           , (verde, green)
           , (grande,  big)
           , (mediano, normal)
           , (chico,   small)
           ]
