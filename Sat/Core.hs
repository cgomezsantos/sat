module Sat.Core where

import Control.Applicative

import Data.Serialize
import qualified Data.Map as M
import qualified Data.Set as S


data Variable = Variable String
    deriving (Eq,Ord,Show)
    
instance Serialize Variable where
    put (Variable str) = put str
    get = Variable <$> get

data Constant = Constant String
    deriving (Eq,Ord,Show)

instance Serialize Constant where
    put (Constant str) = put str
    get = Constant <$> get
    
conName :: Constant -> String
conName (Constant c) = c

data Function = Function { fname :: String
                         , farity :: Int
                         }
    deriving (Eq,Ord,Show)
    
instance Serialize Function where
    put (Function name arity) = put name >> put arity
    get = Function <$> get <*> get

data Relation = Relation { rname :: String
                         , rarity :: Int
                         }
    deriving (Eq,Ord,Show)

instance Serialize Relation where
    put (Relation name arity) = put name >> put arity
    get = Relation <$> get <*> get
    
-- Los predicados son relaciones unarias, pero las representamos
-- con un tipo de datos separado ya que tendrán especial importancia.
data Predicate = Predicate { pname :: String }
    deriving (Eq,Ord,Show)
    
instance Serialize Predicate where
    put (Predicate str) = put str
    get = Predicate <$> get

data Signature = Signature { constants  :: S.Set Constant
                           , functions  :: S.Set Function
                           , predicates :: S.Set Predicate
                           , relations  :: S.Set Relation
                           }

instance Serialize Signature where
    put (Signature c f p r) = put c >> put f >> put p >> put r
    get = Signature <$> get <*> get <*> get <*> get

data Term = Var Variable | Con Constant | Fun Function [Term]
    deriving Show

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
             | Pred Predicate Term 
             | Rel Relation [Term]
    deriving Show

-- Un Modelo es una interpretación de una signatura, dentro de un universo.
-- "subuniv" es el subconjunto (en gral finito) de los elementos del universo
-- que ocurren en las interpretaciones de la signatura.
data Model univ = Model { interpConstants :: M.Map Constant univ
                        , interpFunctions :: M.Map Function ([univ] -> univ)
                        , interpRelations :: M.Map Relation [[univ]]
                        , interpPredicates :: M.Map Predicate [univ]
                        , subuniv :: [univ]
                        }

instance (Show u) => Show (Model u) where
    show (Model { interpConstants = ic
           , interpFunctions = _
           , interpRelations = ir
           , interpPredicates = ip
           , subuniv = su
           }) = "Model \n\tConstants= {"++ show ic ++"}\n\t"++
                         "Relations= {"++ show ir ++"}\n\t"++
                         "Predicates= {"++ show ip ++"}\n\t"++
                         "subuniv= {"++ show su ++"}\n\t"

type Env a = M.Map Variable a

evalTerm :: Model a -> Env a -> Term -> a
evalTerm _ e (Var v) = maybe (error "Variable libre") id $ M.lookup v e
evalTerm m _ (Con c) = maybe (error "") id $ M.lookup c (interpConstants m)
evalTerm m e (Fun f ts) = maybe (error "") ($ (map (evalTerm m e) ts)) $ M.lookup f (interpFunctions m)

eval :: (Eq a) => Formula -> Model a -> Env a -> Bool
eval FTrue _ _ = True
eval FFalse _ _ = False
eval (And p q) m e = eval p m e && eval q m e
eval (Or p q) m e = eval p m e || eval q m e
eval (Impl p q) m e = eval p m e <= eval q m e
eval (Equiv p q) m e = eval p m e == eval q m e
eval (Neg p) m e = not $ eval p m e
eval (ForAll v p) m e = and $ map (\a -> eval p m (M.insert v a e)) (subuniv m)
eval (Exist v p) m e = or $ map (\a -> eval p m (M.insert v a e)) (subuniv m)
eval (Pred p t) m e = maybe False (any ((==) $ evalTerm m e t)) $ M.lookup p (interpPredicates m)
eval (Rel r ts) m e = maybe False (any (map (evalTerm m e) ts ==)) $ M.lookup r (interpRelations m)
