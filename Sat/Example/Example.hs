module Sat.Example.Example where


import Sat.Core
import Sat.Signatures.Figures
import Sat.VisualModels.FiguresBoard
import Sat.VisualModel (interpVisualPredicates)

import qualified Data.Map as M

-- Elementos:

e1 = ElemBoard 1 [triangulo,chico,rojo]
e2 = ElemBoard 2 [cuadrado,grande,verde]
e3 = ElemBoard 3 [triangulo,chico,azul]

b = Board {
            board = [(Coord 0 0,e1),(Coord 1 1,e2),(Coord 2 2,e3)]
          , size = 3
          , bInterpRels = M.fromList [ (derecha,\ls -> xcoord (head ls) >
                                                     xcoord ((head . tail) ls))
                                    , (izquierda,\ls -> xcoord (head ls) <
                                                     xcoord ((head . tail) ls))
                                    , (abajo,\ls -> ycoord (head ls) <
                                                     ycoord ((head . tail) ls))
                                    , (arriba,\ls -> ycoord (head ls) >
                                                     ycoord ((head . tail) ls))]
}



-- Ahora generemos los predicados del modelo:
preds = interpVisualPredicates figuras b



