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
