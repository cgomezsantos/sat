module Sat.Example.Example where


import Sat.Core
import Sat.Parser
import Sat.Signatures.Figures
import Sat.VisualModels.FiguresBoard
import Sat.VisualModel (visualToModel)

import qualified Data.Map as M

-- Elementos:

e1 = ElemBoard 1 Nothing [triangulo,chico,rojo]
e2 = ElemBoard 2 Nothing [cuadrado,grande,verde]
e3 = ElemBoard 3 Nothing [triangulo,chico,azul]
e4 = ElemBoard 4 Nothing [circulo,rojo]
e5 = ElemBoard 5 Nothing [triangulo,rojo]
e6 = ElemBoard 6 Nothing [cuadrado]
e7 = ElemBoard 7 Nothing [cuadrado,chico,verde]
e8 = ElemBoard 8 Nothing [circulo,verde,grande]
e9 = ElemBoard 9 Nothing [triangulo,grande,azul]

b = boardDefault  {
            elems = [ (Coord 0 0,e1)
                    , (Coord 1 1,e2)
                    , (Coord 2 2,e3)
                    , (Coord 3 6,e4)
                    , (Coord 7 7,e5)
                    , (Coord 3 3,e6)
                    , (Coord 4 6,e7)
                    , (Coord 7 0,e8)
                    , (Coord 0 7,e9)
                    ]
          , size = 8
}



-- Ahora generemos el modelo correspondiente a esta configuración de tablero
model = visualToModel b

-- Ahora podríamos evaluar algunas fórmulas:

varx = Variable "x"
vary = Variable "y"

-- Fórmula falsa:
f1 = ForAll varx (Pred triangulo (Var varx))

-- Fórmula Verdadera
f2 = Exist varx (Exist vary (Rel derecha [Var varx,Var vary]))

