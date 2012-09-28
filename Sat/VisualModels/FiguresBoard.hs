{-# Language MultiParamTypeClasses, FunctionalDependencies #-}

module Sat.VisualModels.FiguresBoard where

import Sat.Core
import Sat.VisualModel

import qualified Data.Map as M


-- Un modelo visual concreto para la signatura de las figuras geométricas.
-- Cada vez que se agrega una figura al tablero, se debe crear un ElemBoard
-- con todos los atributos de la figura en forma de predicados (tipo de figura,
-- color, tamaño). La ubicación en el tablero generará luego las relaciones n-arias.

data Coord = Coord {
                xcoord :: Int
              , ycoord :: Int
}
    deriving Eq

data ElemBoard = ElemBoard {
                uElemb :: Int
              , ebPredicates :: [Predicate]
}
    deriving Show

instance ElemVM ElemBoard Int where
    euniv = uElemb
    interpPreds = ebPredicates

-- El mundo será representado como un tablero cuadrado, donde los elementos
-- están ubicados según coordenadas x,y.
data Board = Board {
            board :: [(Coord,ElemBoard)]
          , size :: Int
          -- para cada relación de la signatura definimos un criterio para decidir si n elementos relacionados.
          -- La función asociada a cada relación define la interpretación en el modelo visual.
          , bInterpRels :: M.Map Relation ([Coord] -> Bool)
}
    
instance WorldVM Board ElemBoard Int Coord where
    world = board
    interpRels = bInterpRels
 
   {-
-- un board válido tendrá todos ElemBoard diferentes (es decir con su campo "elem" distinto)
data Board = Board {
                board :: [ElemBoard]
              , size :: Int
              -- para cada relación de la signatura definimos un criterio para decidir si n elementos relacionados.
              -- La función asociada a cada relación define la interpretación en el modelo visual.
              , interpRels :: M.Map Relation ([ElemBoard] -> Bool)
}





triangulo = Predicate "Tr"
cuadrado = Predicate "Cuad"
circulo = Predicate "Circ"


s :: Signature
s = Signature {
        predicates = [triangulo,circulo,cuadrado]
}



b = Board { algo }

elems = map snd (board b)

-- Ahora para generar las relaciones n-arias, estamos muy atados a la signatura
-- en particular.

crearRelDer :: Board -> Relation -> [[univ]]
crearRelDer b r =
    map (\(c,e) -> foldl (\l (c',e') -> if (maybe (error "El Board no se corresponde con la signatura")
                                                  (\f -> f [e,e']) (M.lookup r (interpRels b)))
                                            then [c,c']:l
                                            else l) [] (board b)) (board b)
                                            


crearRelaciones :: Signature -> Board -> M.Map Relation [[univ]]
crearRelaciones s b =
    foldl (\m r -> M.insert r ) M.empty (relations s)
                                            
                                            
-- Genero todas los predicados de la signatura:
getrel p = map elem $ catMaybes $ map (M.lookup p predicates) elems

crearPredicados :: Signature -> [ElemBoard] -> M.Map Predicate [univ]
crearPredicados s elems = 
    foldl (\m p -> M.insert p (getrel p)) M.empty (predicates s)

                  
boardToModel :: Signature -> Board -> Model

            -}      