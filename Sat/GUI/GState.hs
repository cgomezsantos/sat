{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
module Sat.GUI.GState where

import Lens.Family
import Lens.Family.TH

import Graphics.UI.Gtk hiding (get)
import Graphics.Rendering.Cairo (Render)
import Graphics.Rendering.Cairo.SVG (SVG)

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS (RWST,get,put)

import Data.Map (Map)
import Data.IORef (IORef)
import Data.Reference (Reference,newRef,readRef,writeRef)

import Sat.Core
import Sat.VisualModels.FiguresBoard

import Sat.Signatures.Figures

figureList :: [Predicate]
figureList = [triangulo,cuadrado,circulo]

-- | Esto especifica el elemento a agregar en el board. Esto implica
-- llevar la info sobre los predicados del elemento y ademas que elemento
-- del universo va a representar, para calcular ese elemento del universo
-- separamos en los elementos disponibles y el siguiente elemento disponible.
-- En los elementos disponibles estarán los que vayan siendo borrados.
data ElemToAdd = ElemToAdd { _eaPreds  :: [Predicate]
                           , _eaAvails :: [Univ]
                           , _eaMaxId  :: Univ
                           }
$(mkLenses ''ElemToAdd)

-- | Información sobre los items del toolBar.
data SatToolbar = SatToolbar { _symFrameB :: ToggleToolButton }
$(mkLenses ''SatToolbar)

-- | Información sobre la lista de símbolos.
data SatSymList = SatSymList { _gSymFrame    :: Frame
                             , _gGoLeftBox   :: HBox
                             , _gScrollW     :: ScrolledWindow
                             , _gSymIconView :: IconView
                             , _gGoRightBox  :: HBox
                             }
$(mkLenses ''SatSymList)

data GReader = GReader { _gSatFigTable   :: Table
                       , _gSatDrawArea   :: DrawingArea
                       , _gSatPredBox    :: HBox
                       , _gSatSymbolList :: SatSymList
                       , _gSatToolbar    :: SatToolbar
                       }
$(mkLenses ''GReader)

data GState = GState { _gSatBoard         :: Board
                     , _gSatPieceToAdd    :: ElemToAdd
                     }
$(mkLenses ''GState)

-- | Referencia del estado.
type GStateRef = IORef GState

-- | Mónada de la interfaz.
type GuiMonad' = RWST GReader () GStateRef 
type GuiMonad = GuiMonad' IO

io = liftIO

instance Reference IORef GuiMonad where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

-- | Retorna el estado de la mónada de la interfaz.
getGState :: GuiMonad GState
getGState = get >>= readRef

-- | Actualiza el estado de la mónada de la interfaz.
updateGState :: (GState -> GState) -> GuiMonad ()
updateGState f = do
                r <- get
                gst <- readRef r
                writeRef r $ f gst
                put r

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x,f y)
                
getElem :: ListStore a -> TreePath -> IO (Maybe a)
getElem l p = treeModelGetIter l p >>= \i ->
              flip (maybe (return Nothing)) i $ \it -> 
                        (\idx -> listStoreGetSize l >>= \len -> 
                        if idx < len
                            then Just <$> listStoreGetValue l idx
                            else return Nothing) (listStoreIterToIndex it)
