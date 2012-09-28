module Sat.Signatures.Figures where

import Sat.Core

import qualified Data.Set as S


-- Predicados

triangulo = Predicate "Tr"
cuadrado = Predicate "Cuad"
circulo = Predicate "Circ"
chico = Predicate "Chico"
grande = Predicate "Grande"
rojo = Predicate "Rojo"
azul = Predicate "Azul"
verde = Predicate "Verde"

-- Relaciones n-arias

derecha = Relation "der" 2
izquierda = Relation "izq" 2
abajo = Relation "abajo" 2
arriba = Relation "arriba" 2

-- Funciones

siguiente = Function "siguiente" 1


figuras :: Signature
figuras = Signature {
    constants = S.fromList []
  , functions = S.fromList [siguiente]
  , predicates = S.fromList [triangulo,cuadrado,circulo,chico,grande,rojo,azul,verde]
  , relations = S.fromList [derecha,izquierda,abajo,arriba]
}




{-
-- Ejemplo de una formula

form = ForAll (Variable "x") (Pred cuadrado (Var $ Variable "x"))


-- Ejemplo de un modelo

type Universo = Int

grilla1 :: Model Universo
grilla1 = Model {
    interpConstants = M.empty
  , interpFunctions = M.fromList
            [(siguiente,\ as -> case as of
                                  [1] -> 3 
                                  [3] -> 2
                                  [2] ->1)]
  , interpRelations = M.fromList 
            -- Predicados
            [(chico,[[1]])
          , (grande,[[2],[3]])
          , (triangulo,[[1]])
          , (cuadrado,[[2]])
          , (circulo,[[3]])
            -- Relaciones n-arias
          , (derecha,[[1,2],[3,1],[3,2]])
          , (izquierda,[[2,1],[1,3],[2,3]])
          , (arriba,[[1,3],[1,2],[3,2]])
          , (abajo,[[3,1],[2,1],[2,3]])
          ]
}
-}

