module Sat.GUI.File where

import Lens.Family

import qualified Data.Foldable as F
import qualified Data.Serialize as S
import qualified Data.ByteString as B

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS

import Graphics.Rendering.Cairo.SVG

import Sat.GUI.Board
import Sat.GUI.GState

import Sat.VisualModel (visualToModel)
import Sat.VisualModels.FiguresBoard(Board,boardDefault,takeMaxElem)

createNewBoardFromLoad :: Board -> GuiMonad ()
createNewBoardFromLoad board = ask >>= \content -> do
    let model    = visualToModel board
        iconEdit = content ^. gSatIconEditBoard
        da       = content ^. gSatDrawArea 
    
    updateGState ((<~) gSatBoard board)
    updateGState ((<~) (gSatPieceToAdd . eaAvails) [])
    updateGState ((<~) (gSatPieceToAdd . eaMaxId) 0)
    updateGState ((<~) gSatModel model)
    
    io $ widgetHideAll iconEdit
    
    -- Esto es una manera de disparar un evento de Expose para re-dibujar.
    (drawWidth, drawHeight) <- io $ widgetGetSize da
    io $ widgetSetSizeRequest da drawWidth drawHeight
    -- Estaría bueno saber si se puede hacer de otra manera.
    
    return ()

-- | Crea un nuevo archivo en blanco.
createNewBoard :: GuiMonad ()
createNewBoard = createNewBoardFromLoad boardDefault

saveAsBoard :: GuiMonad ()
saveAsBoard = getGState >>= \st -> do
    let board = st ^. gSatBoard
    
    saveDialog "Guardar como" ".sat" satFileFilter board
    
    return ()

loadBoard :: GuiMonad ()
loadBoard = ask >>= \content -> get >>= \s -> do
    
        io $ loadDialog "Cargar" satFileFilter (loadB content s)
    
        return ()
    where
        loadB :: GReader -> GStateRef -> Board -> IO ()
        loadB content s b = evalRWST (do
                let model    = visualToModel b
                    maxid    = takeMaxElem b
                    
                updateGState ((<~) gSatBoard b)
                updateGState ((<~) (gSatPieceToAdd . eaAvails) [])
                updateGState ((<~) (gSatPieceToAdd . eaMaxId) (maxid+1))
                updateGState ((<~) gSatModel model)
                ) content s >> widgetHideAll (content ^. gSatIconEditBoard) 
                            >> return ()

-- Abre una ventana para cargar un tipo con instancia Serialize, retorna True
-- si la opci´on fue cargar, retorna False si la opci´on fue cancelar.
loadDialog :: (S.Serialize s) => String -> (FileChooserDialog -> IO ()) -> 
                               (s -> IO ()) -> IO Bool
loadDialog label fileFilter action = do
    dialog <- fileChooserDialogNew (Just label) 
                                    Nothing 
                                    FileChooserActionOpen
                                    [ ("Cargar",ResponseAccept)
                                    , ("Cancelar",ResponseCancel)]

    fileFilter dialog 
    response <- dialogRun dialog
    
    case response of
        ResponseAccept -> do
            selected <- fileChooserGetFilename dialog
            flip F.mapM_ selected (\filepath -> 
                                    decodeFile filepath >>= \decode ->
                                    action decode >>
                                    widgetDestroy dialog)
            return True
        _ -> widgetDestroy dialog >> return False

-- Abre una ventana para guardar un tipo con instancia Serialize, retorna True 
-- si la opci´on fue guardar, retorna False si la opci´on fue cancelar.
saveDialog :: (S.Serialize s) => String -> String ->
                                 (FileChooserDialog -> IO ()) -> 
                                 s -> GuiMonad Bool
saveDialog label filename fileFilter serialItem = do
    dialog <- io $ fileChooserDialogNew (Just label) 
                                        Nothing 
                                        FileChooserActionSave 
                                        [ ("Guardar",ResponseAccept)
                                        , ("Cancelar",ResponseCancel)]
    
    io $ fileChooserSetCurrentName dialog filename
    io $ fileFilter dialog
    response <- io $ dialogRun dialog

    case response of
        ResponseAccept -> io (fileChooserGetFilename dialog) >>= 
                          \fp -> F.mapM_ save fp >> 
                          io (widgetDestroy dialog) >> return True
        _ -> io (widgetDestroy dialog) >> return False
    where
        save:: FilePath -> GuiMonad ()
        save filepath = io $ encodeFile filepath serialItem

encodeFile :: S.Serialize a => FilePath -> a -> IO ()
encodeFile f v = B.writeFile f (S.encode v)
 
decodeFile :: S.Serialize a => FilePath -> IO a
decodeFile f = B.readFile f >>= 
               either error return .
                      S.runGet (S.get >>= \v ->
                                S.isEmpty >>= \m ->
                                m `seq` return v)

-- | Filtro de programas de sat.
satFileFilter :: (FileChooserClass f, MonadIO m) => f -> m ()
satFileFilter dialog = io $ setFileFilter dialog ["*.sat"] "Programa de sat"

setFileFilter :: FileChooserClass f => f -> [String] -> String -> IO ()
setFileFilter fChooser patterns title = do
    hsfilt <- fileFilterNew
    mapM_ (fileFilterAddPattern hsfilt) patterns
    fileFilterSetName hsfilt title
    fileChooserAddFilter fChooser hsfilt
