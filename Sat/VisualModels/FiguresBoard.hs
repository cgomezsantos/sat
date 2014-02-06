{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
module Sat.VisualModels.FiguresBoard where

import Control.Applicative
import Control.Lens

import Data.Serialize
import qualified Data.Map as M
import qualified Data.Set as S
-- import Data.Maybe
import Data.Function(on)

import Sat.Core
import Sat.VisualModel
import Sat.Signatures.Figures

type Univ = Int

-- Un modelo visual concreto para la signatura de las figuras geométricas.
-- Cada vez que se agrega una figura al tablero, se debe crear un ElemBoard
-- con todos los atributos de la figura en forma de predicados (tipo de figura,
-- color, tamaño). La ubicación en el tablero generará luego las relaciones n-arias.

data Coord = Coord { xcoord :: Int
                   , ycoord :: Int
                   }
    deriving (Eq,Show)

_x :: Lens' Coord Int 
_x = lens xcoord (\p x -> p {xcoord = x })

_y :: Lens' Coord Int
_y = lens ycoord (\p y -> p {xcoord = y })

instance Serialize Coord where
    put (Coord xc yc) = put xc >> put yc
    get = Coord <$> get <*> get

data ElemBoard = ElemBoard { uElemb       :: Int
                           , ebConstant   :: [Constant]
                           , ebPredicates :: [Predicate]
                           }
    deriving Show

ebId :: Lens' ElemBoard Int
ebId =  lens uElemb (\e iden -> e {uElemb = iden })

ebCnst :: Lens' ElemBoard [Constant]
ebCnst = lens ebConstant (\e cnts -> e {ebConstant = cnts })

ebPreds :: Lens' ElemBoard [Predicate]
ebPreds = lens ebPredicates (\e preds -> e {ebPredicates = preds })

instance Eq ElemBoard where
    eb == eb' = uElemb eb == uElemb eb'

instance Serialize ElemBoard where
    put (ElemBoard iden cnts preds) = put iden >> put cnts >> put preds
    get = ElemBoard <$> get <*> get <*> get

instance ElemVM ElemBoard Int where
    euniv       = uElemb
    interpConst = ebConstant
    interpPreds = ebPredicates

type ElemPos = (Coord,ElemBoard)

-- El mundo será representado como un tablero cuadrado, donde los elementos
-- están ubicados según coordenadas x,y.
data Board = Board { elems      :: [ElemPos]
                   , size       :: Int
                   , bsignature :: Signature
                   }

elms :: Lens' Board [ElemPos]
elms = lens elems (\b els -> b {elems = els })


takeMaxElem :: Board -> Univ
takeMaxElem = foldl (\m eb -> if m < uElemb eb 
                                then uElemb eb 
                                else m) 0 . map snd . elems

-- para cada relación de la signatura definimos un criterio para decidir si n elementos relacionados.
-- La función asociada a cada relación define la interpretación en el modelo visual.
bInterpRels :: M.Map Relation ([Coord] -> Bool)
bInterpRels = M.fromList [ (derecha,   comp (>) xcoord)
                         , (izquierda, comp (<) xcoord)
                         , (abajo,     comp (>) ycoord)
                         , (arriba,    comp (<) ycoord)
                         ]
              where comp :: (Int -> Int -> Bool) -> (Coord -> Int) -> [Coord] -> Bool
                    comp ord proj (p1:p2:_) = (ord `on` proj) p1 p2
                    comp _ _ _ = False

makeSignatureWithConstants :: Board -> Signature
makeSignatureWithConstants b = Signature (foldl makeConst S.empty ebs)
                                         (functions sign)
                                         (predicates sign)
                                         (relations sign)
    where
        makeConst :: S.Set Constant -> ElemBoard -> S.Set Constant
        makeConst s eb = s `S.union` S.fromList (ebConstant eb)

        sign :: Signature
        sign = bsignature b
        ebs :: [ElemBoard]
        ebs = map snd (elems b)

instance Serialize Board where
    put (Board es s bsig) = put es >> put s >> put bsig
    get = Board <$> get <*> get <*> get

instance WorldVM Board ElemBoard Int Coord where
    world = elems
    interpRels = const bInterpRels
    signature = makeSignatureWithConstants
