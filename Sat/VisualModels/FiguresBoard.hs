{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
module Sat.VisualModels.FiguresBoard where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Sat.Core
import Sat.VisualModel

import qualified Data.Map as M
import Data.List

type Univ = Int

-- Un modelo visual concreto para la signatura de las figuras geométricas.
-- Cada vez que se agrega una figura al tablero, se debe crear un ElemBoard
-- con todos los atributos de la figura en forma de predicados (tipo de figura,
-- color, tamaño). La ubicación en el tablero generará luego las relaciones n-arias.

data Coord = Coord { xcoord :: Int
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
data Board = Board { board :: [(Coord,ElemBoard)]
                   , size :: Int
                   -- para cada relación de la signatura definimos un criterio para decidir si n elementos relacionados.
                   -- La función asociada a cada relación define la interpretación en el modelo visual.
                   , bInterpRels :: M.Map Relation ([Coord] -> Bool)
                   }

instance WorldVM Board ElemBoard Int Coord where
    world = board
    interpRels = bInterpRels

-- Obtenemos la lista de elementos del universo que cumplen el predicado p.
getElemOfPred :: Predicate -> Board -> [Univ]
getElemOfPred p = map (uElemb . snd) . 
                  filter (maybe False (const True) . find (p==) . ebPredicates . snd) . board

-- Crea los predicados para un modelo.
makePredicates:: Signature -> Board -> M.Map Predicate [Univ]
makePredicates s b = S.foldl 
                        (\m p -> M.insert p (getElemOfPred p b) m) 
                        M.empty 
                        (predicates s)


-- Obtenemos las t-uplas pertenecienes a la relación.
getRelOfBoard :: Relation -> Board -> [[Univ]]
getRelOfBoard r b = foldl tupleInR [] $ relationTUples (rarity r) (board b)
    where
        tupleInR :: [[Univ]] -> [(Coord,ElemBoard)] -> [[Univ]]
        tupleInR l ces = 
                if (maybe (error "El Board no se corresponde con la signatura")
                          (\f -> f (map fst ces)) (M.lookup r (interpRels b)))
                    then (map (uElemb . snd) ces):l
                    else l

-- Crea las relaciones para un modelo.
makeRelations :: Signature -> Board -> M.Map Relation [[Univ]]
makeRelations s b = S.foldl (\m r -> M.insert r (getRelOfBoard r b) m) 
                            M.empty
                            (relations s)

-- Crea un modelo en base a un board.
boardToModel :: Signature -> Board -> [Univ] -> Model Univ
boardToModel s b u = Model M.empty
                           M.empty
                           (makeRelations s b)
                           (makePredicates s b)
                           u

-- Genera las combinaciones de t-uplas de relaciones.
relationTUples :: Int -> [a] -> [[a]]
relationTUples _ [] = [[]]
relationTUples 0 _  = [[]]
relationTUples k xs = [z:ys | z <- xs, ys <- relationTUples (k-1) xs]
