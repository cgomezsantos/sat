-- | Dialogs and actions dealing with files.
module Sat.GUI.File where

import Control.Lens

import qualified Data.Foldable as F
import qualified Data.Serialize as S
import qualified Data.ByteString as B

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS

import Sat.GUI.GState
import Sat.GUI.EntryFormula
import Sat.GUI.FileStatusbar

import Sat.VisualModel (visualToModel)
import Sat.VisualModels.FiguresBoard(Board,takeMaxElem)
import Sat.GUI.Settings (boardDefault)

createNewBoardFromLoad :: Board -> Maybe FilePath -> GuiMonad ()
createNewBoardFromLoad board mfp = ask >>= \content -> do
    let model    = visualToModel board
        maxid    = takeMaxElem board
        da       = content ^. gSatDrawArea 
    
    updateStateField gSatBoard board
    updateStateField (gSatPieceToAdd . eaAvails) []
    updateStateField (gSatPieceToAdd . eaMaxId) (maxid+1)
    updateStateField gSatModel model
    
    case mfp of
        Nothing -> updateStateField gSatFile Nothing >> 
                   updateFileStatusbarNewFile
        Just fp -> updateStateField gSatFile (Just $ SatFile fp) >> 
                   updateFileStatusbar fp
    
    io $ widgetQueueDraw da
    
    return ()

-- | Crea un nuevo archivo en blanco.
createNewBoard :: GuiMonad ()
createNewBoard = createNewBoardFromLoad boardDefault Nothing

saveBoard :: GuiMonad ()
saveBoard = getGState >>= \st -> do
    let mfp   = st ^. gSatFile
        board = st ^. gSatBoard
        flist = st ^. gSatFList
    maybe saveAsBoard (save (board,flist)) mfp
    updateFileStatusbarFileSave
    where
        save:: (Board,[String]) -> SatFile -> GuiMonad ()
        save bflist sfile = io $ encodeFile (sfile ^. gname) bflist

saveAsBoard :: GuiMonad ()
saveAsBoard = getGState >>= \st -> do
    let board = st ^. gSatBoard
        flist = st ^. gSatFList
    
    mfp <- saveDialog "Guardar como" ".sat" satFileFilter (board,flist)
    
    case mfp of
        Nothing -> updateStateField gSatFile Nothing
        Just fp -> updateStateField gSatFile (Just $ SatFile fp)
    
    return ()

loadBoard :: GuiMonad ()
loadBoard = ask >>= \content -> get >>= \s -> do
    
        _ <- io $ loadDialog "Cargar" satFileFilter (loadB content s)
    
        return ()
    where
        loadB :: GReader -> GStateRef -> FilePath -> (Board,[String]) -> IO ()
        loadB content s fp (b,flist) = void $ evalRWST' content s $ do
                createNewBoardFromLoad b (Just fp)
                createNewEntryFormulaList flist

-- Abre una ventana para cargar un tipo con instancia Serialize, retorna True
-- si la opci´on fue cargar, retorna False si la opci´on fue cancelar.
loadDialog :: (S.Serialize s) => String -> (FileChooserDialog -> IO ()) -> 
                               (FilePath -> s -> IO ()) -> IO Bool
loadDialog label fileFilter action = do
    dialog <- fileChooserDialogNew (Just label) 
                                    Nothing 
                                    FileChooserActionOpen
                                    [ ("Cargar",ResponseAccept)
                                    , ("Cancelar",ResponseCancel)]

    fileFilter dialog 
    resp <- dialogRun dialog
    
    case resp of
        ResponseAccept -> do
            selected <- fileChooserGetFilename dialog
            F.mapM_ (\filepath -> 
                    decodeFile filepath >>= \decode ->
                    action filepath decode >>
                    widgetDestroy dialog) selected 
            return True
        _ -> widgetDestroy dialog >> return False

-- Abre una ventana para guardar un tipo con instancia Serialize, retorna True 
-- si la opci´on fue guardar, retorna False si la opci´on fue cancelar.
saveDialog :: (S.Serialize s) => String -> String ->
                                 (FileChooserDialog -> IO ()) -> 
                                 s -> GuiMonad (Maybe FilePath)
saveDialog label filename fileFilter serialItem = do
    dialog <- io $ fileChooserDialogNew (Just label) 
                                        Nothing 
                                        FileChooserActionSave 
                                        [ ("Guardar",ResponseAccept)
                                        , ("Cancelar",ResponseCancel)]
    
    io $ fileChooserSetCurrentName dialog filename
    io $ fileFilter dialog
    resp <- io $ dialogRun dialog

    case resp of
        ResponseAccept -> io (fileChooserGetFilename dialog) >>= 
                          \mfp -> F.mapM_ save mfp >> 
                          io (widgetDestroy dialog) >> return mfp
        _ -> io (widgetDestroy dialog) >> return Nothing
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
satFileFilter dialog = io $ setFileFilter dialog ["*.sat"] "Archivo de sat"

setFileFilter :: FileChooserClass f => f -> [String] -> String -> IO ()
setFileFilter fChooser patterns title = do
    hsfilt <- fileFilterNew
    mapM_ (fileFilterAddPattern hsfilt) patterns
    fileFilterSetName hsfilt title
    fileChooserAddFilter fChooser hsfilt
