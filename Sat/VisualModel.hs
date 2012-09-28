{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
module Sat.VisualModel where

import Sat.Core

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
   
   
{- Un ElemVM será un elemento del universo representado visualmente.
   Contiene la información de qué predicados de la signatura satisface.
   Por ejemplo, un triángulo chico y rojo, tendrá en la lsta interpPreds
   a los predicados chico y rojo solamente.
-}
class ElemVM e univ | e -> univ where
    euniv :: e -> univ
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
class (ElemVM e univ, Eq coord) => WorldVM b e univ coord | b -> e, b -> coord, e -> univ where
    world :: b -> [(coord,e)]
    interpRels :: b -> M.Map Relation ([coord] -> Bool)
    
    
interpVisualPredicates :: (WorldVM w e univ coord) =>
                          Signature -> w -> M.Map Predicate [univ]
interpVisualPredicates s w = 
    S.foldl (\m p -> M.insert p (getpreds p) m) M.empty (predicates s)
    
    where getpreds p = map euniv $ filter ((elem p) . interpPreds) elems
          elems = map snd (world w)
          


interpVisualRelations :: (WorldVM w e univ coord) =>
                         Signature -> w -> M.Map Relation [[univ]]
interpVisualRelations = undefined
          
          
          
