{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
module Sat.VisualModel (
      ElemVM(..)
    , WorldVM(..)
    , visualToModel
    , getElems
    , interpVisualPredicates
    , interpVisualRelations
    )
    where

import Sat.Core

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

{- Un ElemVM será un elemento del universo representado visualmente.
   Contiene la información de qué predicados de la signatura satisface.
   Por ejemplo, un triángulo chico y rojo, tendrá en la lsta interpPreds
   a los predicados chico y rojo solamente.
-}
class (Eq univ) => ElemVM e univ | e -> univ where
    euniv       :: e -> univ
    interpConst :: e -> Maybe Constant
    interpPreds :: e -> [Predicate]
    
   
{- El "mundo" en el modelo visual será el lugar donde se ubicarán los elementos
   del universo, para generar modelos de una signatura.
   Las relaciones n-arias quedarán determinadas con respecto a la manera en que 
   se ubican estos elementos en el mundo. Por ejemplo en un tablero cuadrado,
   teniendo las relaciones "derecha" e "izquierda" se crean de acuerdo a la posición
   "x" de cada par de elementos que se encuentran en el mundo.
   La función de tipo "[e] -> Bool" será un criterio para decidir si n elementos
   están relacionados en cada relación n-aria.
   Las coordenadas será un tipo de datos que representa la posición de cada elemento
   en el mundo.
   -}
class (Eq univ, ElemVM e univ, Eq coord) => WorldVM b e univ coord | b -> e, b -> coord, e -> univ where
    world      :: b -> [(coord,e)]
    interpRels :: b -> M.Map Relation ([coord] -> Bool)
    signature  :: b -> Signature
    
getElems :: (WorldVM w e univ coord) => w -> [univ]
getElems = map (euniv . snd) . world

interpVisualConstants :: (WorldVM w e univ coord) => w -> M.Map Constant univ
interpVisualConstants w = 
        foldl (\m eb -> 
                maybe m (\c -> M.insert c (euniv eb) m) (interpConst eb)
              ) M.empty elems
    
    where elems = map snd (world w)
    
interpVisualPredicates :: (WorldVM w e univ coord) => w -> M.Map Predicate [univ]
interpVisualPredicates w = 
    S.foldl (\m p -> M.insert p (getpreds p) m) M.empty (predicates s)
    
    where getpreds p = map euniv $ filter ((elem p) . interpPreds) elems
          elems = map snd (world w)
          s = signature w

interpVisualRelations :: (WorldVM w e univ coord) => w -> M.Map Relation [[univ]]
interpVisualRelations w = S.foldl (\m r -> M.insert r (getRelOfWorld r w) m) 
                            M.empty
                            (relations s)
    where s = signature w
          
          
-- Obtenemos las t-uplas pertenecienes a la relación.
getRelOfWorld :: (WorldVM w e univ coord) => Relation -> w -> [[univ]]
getRelOfWorld r w = foldl tupleInR [] $ relationTUples (rarity r) (world w)
    where
        --tupleInR :: [[univ]] -> [(Coord,ElemBoard)] -> [[univ]]
        tupleInR l ces = 
                if (maybe (error "El Modelo Visual no se corresponde con la Signatura")
                          (\f -> f (map fst ces)) (M.lookup r (interpRels w)))
                    then (map (euniv . snd) ces):l
                    else l

                            
-- Crea un modelo en base a un modelo visual.
visualToModel :: (WorldVM w e univ coord) => 
                w -> Model univ
visualToModel w = Model (interpVisualConstants w)
                        M.empty
                        (interpVisualRelations w)
                        (interpVisualPredicates w)
                        (getElems w)

-- Genera las combinaciones de t-uplas de relaciones.
relationTUples :: Int -> [a] -> [[a]]
relationTUples _ [] = [[]]
relationTUples 0 _  = [[]]
relationTUples k xs = [z:ys | z <- xs, ys <- relationTUples (k-1) xs]
