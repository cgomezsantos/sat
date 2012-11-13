module Sat.Signatures.Figures where

import Sat.Core

import qualified Data.Set as S


-- Predicados

triangulo = Predicate "Tr" True
cuadrado = Predicate "Cuad" True
circulo = Predicate "Circ" True
chico = Predicate "Chico" False
mediano = Predicate "Mediano" False
grande = Predicate "Grande" False
rojo = Predicate "Rojo" False
azul = Predicate "Azul" False
verde = Predicate "Verde" False

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
  , predicates = S.fromList [ triangulo, cuadrado, circulo
                            , chico, mediano, grande
                            , rojo, azul, verde
                            ]
  , relations = S.fromList [derecha,izquierda,abajo,arriba]
}
