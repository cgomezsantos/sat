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

import Sat.VisualModels.FiguresBoard

type PiecesToDraw = Map Univ (Render Bool)

type EditSVG = SVG -> SVG

data FigureItem = FigureItem { _fiName   :: String
                             , _fiSVG    :: SVG
                             , _fiPixbuf :: Pixbuf 
                             }
$(mkLenses ''FigureItem)

-- | Tenemos la información sobre, el nombre del icono, la figura del icono
-- y que acción toma en la edición de un SVG.
data PredicateItem = PredicateItem { _piName   :: Maybe String
                                   , _piPixbuf :: Maybe Pixbuf
                                   , _piTrans  :: EditSVG
                                   }
$(mkLenses ''PredicateItem)

data PieceToAdd = PieceToAdd { _paFig   :: Maybe FigureItem 
                             , _paPreds :: Map IconView (Maybe PredicateItem)
                             }
$(mkLenses ''PieceToAdd)

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

data GReader = GReader { _gSatFigIV      :: IconView
                       , _gSatDrawArea   :: DrawingArea
                       , _gSatPredBox    :: HBox 
                       , _gSatSymbolList :: SatSymList
                       , _gSatToolbar    :: SatToolbar
                       }
$(mkLenses ''GReader)

data GState = GState { _gSatBoard         :: Board
                     , _gSatPieceToAdd    :: PieceToAdd
                     , _gSatPiecesInBoard :: PiecesToDraw
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
