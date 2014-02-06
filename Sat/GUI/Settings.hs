-- | Configuraciones 
module Sat.GUI.Settings where

import Sat.VisualModels.FiguresBoard
import Sat.Signatures.Figures


-- El tablero default contiene las funciones para definir las relaciones:
boardDefault :: Board
boardDefault = Board { elems = []
                     , size = 8
                     , bsignature = figuras
}

boardWidth :: Int
boardWidth = size boardDefault 

