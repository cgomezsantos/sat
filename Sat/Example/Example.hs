module Sat.Example.Example where


import Sat.Core
import Sat.Signatures.Figures
import Sat.VisualModels.FiguresBoard
import Sat.VisualModel (visualToModel)

import qualified Data.Map as M

-- Elementos:

e1 = ElemBoard 1 [triangulo,chico,rojo]
e2 = ElemBoard 2 [cuadrado,grande,verde]
e3 = ElemBoard 3 [triangulo,chico,azul]
e4 = ElemBoard 4 [circulo,rojo]

b = boardDefault  {
            elems = [(Coord 0 0,e1),(Coord 1 1,e2),(Coord 2 2,e3),(Coord 3 6,e4)]
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

