{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
module Sat.GUI.GState where

import Lens.Family
import Lens.Family.TH

import Graphics.UI.Gtk hiding (get)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS (RWST,get,put)
import Data.IORef (IORef)
import Data.Reference (Reference,newRef,readRef,writeRef)

import Sat.VisualModels.FiguresBoard

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

data GState = GState { _gSatBoard :: Board }
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
