-- | State of the GUI that represent abstractly the visual world.
{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
module Sat.GUI.GState where

import Lens.Family
import Lens.Family.TH

import Graphics.UI.Gtk hiding (get)


import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad.Trans.RWS (RWST,get,put,evalRWST)


import Data.IORef (IORef)
import Data.Reference (Reference,newRef,readRef,writeRef)

import Sat.Core
import Sat.VisualModels.FiguresBoard



-- | Esto especifica el elemento a agregar en el board. Esto implica
-- llevar la info sobre los predicados del elemento y ademas que elemento
-- del universo va a representar, para calcular ese elemento del universo
-- separamos en los elementos disponibles y el siguiente elemento disponible.
-- En los elementos disponibles estarán los que vayan siendo borrados.
data ElemToAdd = ElemToAdd { _eaPreds  :: [Predicate]
                           , _eaAvails :: [Univ]
                           , _eaMaxId  :: Univ
                           }
    deriving Show
$(mkLenses ''ElemToAdd)

-- | Información sobre la lista de símbolos.
-- Ahora no lo usamos, SI NO LO VAMOS A QUERER, ENTONCES BORRAR ESTE TIPO.
data SatSymList = SatSymList { _gSymFrame    :: Frame
                             , _gGoLeftBox   :: HBox
                             , _gScrollW     :: ScrolledWindow
                             , _gSymIconView :: IconView
                             , _gGoRightBox  :: HBox
                             }
$(mkLenses ''SatSymList)

data SatTVFormulaItem = SatTVFormulaItem { _gBoxTreeView  :: ScrolledWindow
                                         , _gAddFButton   :: ToolButton
                                         , _gDelFButton   :: ToolButton
                                         , _gCheckFButton :: ToolButton
                                         }
$(mkLenses ''SatTVFormulaItem)

data SatFile = SatFile { _gname :: FilePath }
$(mkLenses ''SatFile)


-- | Undo- Redo
data URInfo = URInfo { _urBoard :: Board
                     , _urFList :: [String]
                     , _urPieceToAdd :: ElemToAdd
}
$(mkLenses ''URInfo)

-- | El estado de undo-redo es una lista de acciones, y el índice de qué URInfo es la que está
--   actualmente cargada al GState.
type URState = ([URInfo],Int)

data GReader = GReader { _gSatWindow        :: Window 
                       , _gSatFigTable      :: Table
                       , _gSatDrawArea      :: DrawingArea
                       , _gSatPrevFigDA     :: DrawingArea
                       , _gSatInfoStatusbar :: Statusbar
                       , _gSatFileStatusbar :: Statusbar
                       , _gSatPredBox       :: HBox
                       , _gSatTVFormula     :: SatTVFormulaItem
                       }
$(mkLenses ''GReader)

data GState = GState { _gSatBoard       :: Board
                     , _gSatFList       :: [String]
                     , _gSatPieceToAdd  :: ElemToAdd
                     , _gSatModel       :: Model Univ
                     , _gSatFile        :: Maybe SatFile
                     , _gSatDNDSrcCoord :: Maybe (ElemBoard,Int,Int)
                     , _gURState        :: URState
                     }
$(mkLenses ''GState)

-- | Referencia del estado.
type GStateRef = IORef GState

-- | Mónada de la interfaz.
type GuiMonad' = RWST GReader () GStateRef 
type GuiMonad = GuiMonad' IO

io :: Control.Monad.IO.Class.MonadIO m => IO a -> m a
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


mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f = f *** f

getElem :: ListStore a -> TreePath -> IO (Maybe a)
getElem l p = treeModelGetIter l p >>= \i ->
              flip (maybe (return Nothing)) i $ \it -> 
                        (\idx -> listStoreGetSize l >>= \len -> 
                        if idx < len
                            then Just <$> listStoreGetValue l idx
                            else return Nothing) (listStoreIterToIndex it)

evalGState :: Monad m => r -> s -> RWST r w s m a -> m a
evalGState content state action = evalRWST action content state >>= return . fst

evalRWST' :: Monad m => r -> s -> RWST r w s m a -> m (a, w)
evalRWST' content state action = evalRWST action content state


-- | Función para agregar una nueva acción a la lista de undos.
--   Si ya habíamos hecho undo, entonces al hacer una nueva acción no se puede hacer redo.
--   Por eso, el índice se vuelve a 0 y se tira la parte de la lista correspondiente a los redos.
--   Debería estar en UndoRedo, pero no puedo por imports ciclicos
addToUndo :: GuiMonad ()
addToUndo = 
    getGState >>= \st ->
    do
        let board = st ^. gSatBoard
            flist = st ^. gSatFList
            eToAdd = st ^. gSatPieceToAdd
            
            urInfo = URInfo board flist eToAdd
            
        updateGState ((%~) gURState (updateURState urInfo))
        
    where updateURState urinfo (listundo,i) = (urinfo:(drop i listundo),0)

                      
