module Sat.Signatures.Figures where

import Sat.Core

import qualified Data.Set as S


-- Predicados
triangulo,cuadrado,circulo :: Predicate
triangulo = Predicate "Tr" True
cuadrado = Predicate "Cuad" True
circulo = Predicate "Circ" True

chico,mediano,grande :: Predicate
chico = Predicate "Chico" False
mediano = Predicate "Mediano" False
grande = Predicate "Grande" False

rojo,azul,verde :: Predicate
rojo = Predicate "Rojo" False
azul = Predicate "Azul" False
verde = Predicate "Verde" False

-- Relaciones n-arias
derecha,izquierda,abajo,arriba :: Relation
derecha = Relation "der" 2
izquierda = Relation "izq" 2
abajo = Relation "abajo" 2
arriba = Relation "arriba" 2

-- Funciones


figuras :: Signature
figuras = Signature {
    constants = S.empty
  , functions = S.empty
  , predicates = S.fromList [ triangulo, cuadrado, circulo
                            , chico, mediano, grande
                            , rojo, azul, verde
                            ]
  , relations = S.fromList [derecha,izquierda,abajo,arriba]
}
