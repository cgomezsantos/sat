-- | Definition of the syntax of formulas of first order, signatures,
-- and models; evaluation of formulas under an interpretation for the 
-- signature.
module Sat.Core where

import Control.Applicative
import Control.Lens

import Data.Serialize
import qualified Data.Map as M
import qualified Data.Set as S

-- Variable is string?
data Variable = Variable String
    deriving (Eq,Ord,Show)
    
instance Serialize Variable where
    put (Variable str) = put str
    get = Variable <$> get

data Constant = Constant String
    deriving (Eq,Ord,Show)
    
constName :: Constant -> String
constName (Constant str) = str

constn :: Lens' Constant String
constn = lens constName (const Constant)

strConst :: Lens' String Constant
strConst = lens Constant (const constName)

instance Serialize Constant where
    put (Constant str) = put str
    get = Constant <$> get
    
conName :: Constant -> String
conName = (^. constn)

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
data Predicate = Predicate { pname :: String 
                           , pmain :: Bool
                           }
    deriving (Eq,Ord,Show)
    
instance Serialize Predicate where
    put (Predicate str b) = put str >> put b
    get = Predicate <$> get <*> get

data Signature = Signature { constants  :: S.Set Constant
                           , functions  :: S.Set Function
                           , predicates :: S.Set Predicate
                           , relations  :: S.Set Relation
                           }

instance Serialize Signature where
    put (Signature c f p r) = put c >> put f >> put p >> put r
    get = Signature <$> get <*> get <*> get <*> get

-- | Terms.
data Term = Var Variable | Con Constant | Fun Function [Term]
    deriving Show

isTermOfTao :: Term -> Signature -> Bool
isTermOfTao (Var _) _ = True
isTermOfTao (Con c) s = S.member c (constants s)
isTermOfTao (Fun f terms) s =
    S.member f (functions s) &&
    length terms == (farity f) &&
    all (flip isTermOfTao s) terms



freeVars :: Term -> S.Set Variable
freeVars (Var v) = S.singleton v
freeVars (Con _) = S.empty
freeVars (Fun _ ts) = S.unions $ map freeVars ts


-- | Well-formed formulas.
data Formula = FTrue | FFalse 
             | Eq Term Term 
             | Neg Formula 
             | And Formula Formula  | Or Formula Formula
             | Impl Formula Formula | Equiv Formula Formula 
             | ForAll Variable Formula | Exist Variable Formula
             | Pred Predicate Term 
             | Rel Relation [Term]
    deriving Show

-- trying to define isFormOfTao
isFormOfTao :: Formula -> Signature -> Bool
isFormOfTao (FTrue) _ = True
isFormOfTao (FFalse) _ = True
isFormOfTao (Eq t t') s = isTermOfTao t s && isTermOfTao t' s
isFormOfTao (Neg f) s = isFormOfTao f s
isFormOfTao (And f f') s = isFormOfTao f s && isFormOfTao f' s 
isFormOfTao (Or f f') s = isFormOfTao f s && isFormOfTao f' s 
isFormOfTao (Impl f f') s = isFormOfTao f s && isFormOfTao f' s 
isFormOfTao (Equiv f f') s = isFormOfTao f s && isFormOfTao f' s 
isFormOfTao (ForAll v f) s = isFormOfTao f s
isFormOfTao (Exist v f) s = isFormOfTao f s
isFormOfTao (Pred p t) s = S.member p (predicates s) && isTermOfTao t s
isFormOfTao (Rel r terms) = S.member r (relations s) && all (flip isTermOfTao s) terms


-- | We can only evaluate closed formulas
isClosed :: Formula -> Bool
isClosed = isClosed' S.empty
  where isClosed' :: S.Set Variable -> Formula -> Bool
        isClosed' _ FTrue = True
        isClosed' _ FFalse = True
        isClosed' bv (Pred _ t) = S.null $ freeVars t S.\\ bv
        isClosed' bv (Rel _ ts) = S.null $ (S.unions $ map freeVars ts) S.\\ bv
        isClosed' bv (Eq t t') = S.null $ (freeVars t `S.union` freeVars t') S.\\ bv
        isClosed' bv (Neg f) = isClosed' bv f
        isClosed' bv (And f f') = isClosed' bv f && isClosed' bv f'
        isClosed' bv (Or f f') = isClosed' bv f && isClosed' bv f'
        isClosed' bv (Impl f f') = isClosed' bv f && isClosed' bv f'
        isClosed' bv (Equiv f f') = isClosed' bv f && isClosed' bv f'
        isClosed' bv (ForAll v f) = isClosed' (S.insert v bv) f
        isClosed' bv (Exist v f) = isClosed' (S.insert v bv) f



-- Un Modelo es una interpretación de una signatura, dentro de un universo.
-- "subuniv" es el subconjunto (en gral finito) de los elementos del universo
-- que ocurren en las interpretaciones de la signatura.

-- | Model with the universe of discourse as a parameter.
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

-- | Interpretation for free-variables.
type Env a = M.Map Variable a

-- | Evaluation of terms under an environment.
evalTerm :: Model a -> Env a -> Term -> a
evalTerm _ e (Var v) = maybe (error "Variable libre") id $ M.lookup v e
evalTerm m _ (Con c) = maybe (error "") id $ M.lookup c (interpConstants m)
evalTerm m e (Fun f ts) = maybe (error "") ($ (map (evalTerm m e) ts)) $ M.lookup f (interpFunctions m)

-- | Evaluation of formulas under a model and an environment. This
-- function is total only for finite models.
eval :: (Eq a) => Formula -> Model a -> Env a -> Bool
eval FTrue _ _ = True
eval FFalse _ _ = False
eval (Eq t t') m e = evalTerm m e t == evalTerm m e t' 
eval (And p q) m e = eval p m e && eval q m e
eval (Or p q) m e = eval p m e || eval q m e
eval (Impl p q) m e = eval p m e <= eval q m e
eval (Equiv p q) m e = eval p m e == eval q m e
eval (Neg p) m e = not $ eval p m e
eval (ForAll v p) m e = and $ map (\a -> eval p m (M.insert v a e)) (subuniv m)
eval (Exist v p) m e = or $ map (\a -> eval p m (M.insert v a e)) (subuniv m)
eval (Pred p t) m e = maybe False (any ((==) $ evalTerm m e t)) $ M.lookup p (interpPredicates m)
eval (Rel r ts) m e = maybe False (any (map (evalTerm m e) ts ==)) $ M.lookup r (interpRelations m)
