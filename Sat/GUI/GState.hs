-- | State of the GUI that represent abstractly the visual world.
{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
module Sat.GUI.GState where


import Control.Lens

import Graphics.UI.Gtk hiding (get)

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad.Trans.RWS (RWST,get,evalRWST)


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
$(makeLenses ''ElemToAdd)

-- | Información sobre la lista de símbolos.
-- Ahora no lo usamos, SI NO LO VAMOS A QUERER, ENTONCES BORRAR ESTE TIPO.
data SatSymList = SatSymList { _gSymFrame    :: Frame
                             , _gGoLeftBox   :: HBox
                             , _gScrollW     :: ScrolledWindow
                             , _gSymIconView :: IconView
                             , _gGoRightBox  :: HBox
                             }
$(makeLenses ''SatSymList)

data SatTVFormulaItem = SatTVFormulaItem { _gBoxTreeView  :: ScrolledWindow
                                         , _gAddFButton   :: ToolButton
                                         , _gDelFButton   :: ToolButton
                                         , _gCheckFButton :: ToolButton
                                         }
$(makeLenses ''SatTVFormulaItem)

data SatFile = SatFile { _gname :: FilePath }
$(makeLenses ''SatFile)


-- | Undo- Redo
data URInfo = URInfo { _urBoard :: Board
                     , _urFList :: [String]
                     , _urPieceToAdd :: ElemToAdd
}
$(makeLenses ''URInfo)

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
$(makeLenses ''GReader)

data GState = GState { _gSatBoard       :: Board
                     , _gSatFList       :: [String]
                     , _gSatPieceToAdd  :: ElemToAdd
                     , _gSatModel       :: Model Univ
                     , _gSatFile        :: Maybe SatFile
                     , _gSatDNDSrcCoord :: Maybe ElemPos
                     , _gURState        :: URState
                     }
$(makeLenses ''GState)

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

useG :: Getting b GState b -> GuiMonad b
useG v = getGState >>= \st -> return $ st ^. v

-- | Actualiza el estado de la mónada de la interfaz.
updateGState :: (GState -> GState) -> GuiMonad ()
updateGState f = do
                r <- get
                gst <- readRef r
                writeRef r $ f gst


--updateStateField :: (Lens' GState a) -> a -> GuiMonad ()
updateStateField :: ASetter GState GState a b -> b -> GuiMonad ()
updateStateField field value = updateGState (\st -> st & field .~ value)

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f t = t & both %~ f

whenM :: Maybe a -> b -> (a -> b) -> b
whenM may dont does = maybe dont does may


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

flipEvalRWST :: Monad m => r -> s -> RWST r w s m a -> m (a, w)
flipEvalRWST r s rwst = evalRWST rwst r s


-- | Función para agregar una nueva acción a la lista de undos.
--   Si ya habíamos hecho undo, entonces al hacer una nueva acción no se puede hacer redo.
--   Por eso, el índice se vuelve a 0 y se tira la parte de la lista correspondiente a los redos.
--   Debería estar en UndoRedo, pero no puedo por imports ciclicos
addToUndo :: GuiMonad ()
addToUndo = do  board <- useG gSatBoard
                flist <- useG gSatFList
                eToAdd <- useG gSatPieceToAdd
                let urInfo = URInfo board flist eToAdd
                updateGState ((%~) gURState (updateURState urInfo))
        
    where updateURState urinfo (listundo,i) = (urinfo:(drop i listundo),0)

                      
newElem :: GuiMonad (Univ,[Univ])
newElem = useG (gSatPieceToAdd . eaAvails) >>= \avails ->
          useG (gSatPieceToAdd . eaMaxId) >>= \i ->
          return (if null avails
                 then (i + 1,avails)
                 else (i,tail avails))
