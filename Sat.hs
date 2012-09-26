import qualified Data.Map as M
import qualified Data.Set as S


data Variable = Variable String
    deriving (Eq,Ord,Show)

data Constant = Constant String
    deriving (Eq,Ord,Show)

data Function = Function {
            fname :: String
          , farity :: Int
}
    deriving (Eq,Ord,Show)

data Relation = Relation {
            rname :: String
          , rarity :: Int
}
    deriving (Eq,Ord,Show)


data Signature = Signature {
           constants :: S.Set Constant
         , functions :: S.Set Function
         , relations :: S.Set Relation
}


data Term = Var Variable | Con Constant | Fun Function [Term]

isTermOfTao :: Term -> Signature -> Bool
isTermOfTao (Var v) _ = True
isTermOfTao (Con c) s = S.member c (constants s)
isTermOfTao (Fun f terms) s =
    S.member f (functions s) &&
    length terms == (farity f) &&
    all (flip isTermOfTao s) terms

data Formula = FTrue | FFalse | And Formula Formula | Or Formula Formula
             | Impl Formula Formula| Equiv Formula Formula | Neg Formula 
             | ForAll Variable Formula | Exist Variable Formula
             | Pred Relation Term 
             | Rel Relation [Term]


data Model univ = Model {
           interpConstants :: M.Map Constant univ
         , interpFunctions :: M.Map Function ([univ] -> univ)
         , interpRelations :: M.Map Relation [[univ]]
}

type Env a = M.Map Variable a


evalTerm :: Model a -> Env a -> Term -> a
evalTerm _ e (Var v) = maybe (error "") id $ M.lookup v e
evalTerm m _ (Con c) = maybe (error "") id $ M.lookup c (interpConstants m)
evalTerm m e (Fun f ts) = maybe (error "") ($ (map (evalTerm m e) ts)) $ M.lookup f (interpFunctions m)

eval :: (Eq a,Enum a,Bounded a) => Formula -> Model a -> Env a -> Bool
eval FTrue _ _ = True
eval FFalse _ _ = False
eval (And p q) m e = eval p m e && eval q m e
eval (Or p q) m e = eval p m e || eval q m e
eval (Impl p q) m e = eval p m e <= eval q m e
eval (Equiv p q) m e = eval p m e == eval q m e
eval (Neg p) m e = not $ eval p m e
eval (ForAll v p) m e = and [eval p m (M.insert v (toEnum a) e) | a <- [1..]]
eval (Exist v p) m e = or [eval p m (M.insert v (toEnum a) e) | a <- [1..]]
eval (Pred r t) m e = maybe False (any (evalTerm m e t `elem`)) $ M.lookup r (interpRelations m) 
eval (Rel r ts) m e = maybe False (any (map (evalTerm m e) ts ==)) $ M.lookup r (interpRelations m) 

-- EJEMPLO:

-- Como ejemplo de funcion, si pensamos en una grilla, podriamos numerar todos los
-- casilleros y entonces esta funcion devuelve el elemento ubicado en la casilla siguiente.
siguiente = Function "siguiente" 1

-- Predicados

triangulo = Relation "Tr" 1
cuadrado = Relation "Cuad" 1
circulo = Relation "Circ" 1
chico = Relation "Chico" 1
grande = Relation "Grande" 1

-- Relaciones n-arias

derecha = Relation "der" 2
izquierda = Relation "izq" 2
abajo = Relation "abajo" 2
arriba = Relation "arriba" 2


figuras :: Signature
figuras = Signature {
    constants = S.fromList []
  , functions = S.fromList [siguiente]
  , relations = S.fromList [triangulo,cuadrado,circulo,derecha,abajo,arriba]
}

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

