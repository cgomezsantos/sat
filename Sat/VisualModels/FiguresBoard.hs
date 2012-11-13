{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
module Sat.VisualModels.FiguresBoard where

import Control.Applicative

import Data.Serialize
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Function(on)

import Sat.Core
import Sat.VisualModel
import Sat.Signatures.Figures

import qualified Data.Map as M
import Data.List

type Univ = Int

-- Un modelo visual concreto para la signatura de las figuras geométricas.
-- Cada vez que se agrega una figura al tablero, se debe crear un ElemBoard
-- con todos los atributos de la figura en forma de predicados (tipo de figura,
-- color, tamaño). La ubicación en el tablero generará luego las relaciones n-arias.

data Coord = Coord { xcoord :: Int
                   , ycoord :: Int
                   }
    deriving Eq
    
instance Serialize Coord where
    put (Coord xc yc) = put xc >> put yc
    get = Coord <$> get <*> get

data ElemBoard = ElemBoard { uElemb :: Int
                           , ebPredicates  :: [Predicate]
                           }
    deriving (Show,Eq)
    
instance Serialize ElemBoard where
    put (ElemBoard ue ebPreds) = put ue >> put ebPreds
    get = ElemBoard <$> get <*> get

instance ElemVM ElemBoard Int where
    euniv = uElemb
    interpPreds = ebPredicates

-- El mundo será representado como un tablero cuadrado, donde los elementos
-- están ubicados según coordenadas x,y.
data Board = Board { elems :: [(Coord,ElemBoard)]
                   , size :: Int
                   , bsignature :: Signature
                   }

-- El tablero default contiene las funciones para definir las relaciones:
boardDefault = Board { elems = []
                     , size = 8
                     , bsignature = figuras
}

takeMaxElem :: Board -> Univ
takeMaxElem = foldl (\m eb -> if m < uElemb eb 
                                then uElemb eb 
                                else m) 0 . map snd . elems

-- para cada relación de la signatura definimos un criterio para decidir si n elementos relacionados.
-- La función asociada a cada relación define la interpretación en el modelo visual.
bInterpRels :: M.Map Relation ([Coord] -> Bool)
bInterpRels = M.fromList [ (derecha,   comp (>) xcoord)
                         , (izquierda, comp (<) xcoord)
                         , (abajo,     comp (<) ycoord)
                         , (arriba,    comp (>) ycoord)
                         ]
              where comp :: (Int -> Int -> Bool) -> (Coord -> Int) -> [Coord] -> Bool
                    comp ord proj (p1:p2:_) = (ord `on` proj) p1 p2
                    comp _ _ _ = False

instance Serialize Board where
    put (Board es s bsig) = put es >> put s >> put bsig
    get = Board <$> get <*> get <*> get

instance WorldVM Board ElemBoard Int Coord where
    world = elems
    interpRels = const bInterpRels
    signature = bsignature
