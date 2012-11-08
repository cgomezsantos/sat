module Sat.GUI.File where

import Lens.Family

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)

import Control.Monad.Trans.RWS

import Graphics.Rendering.Cairo.SVG

import Sat.GUI.Board
import Sat.GUI.GState

import Sat.VisualModel (visualToModel)
import Sat.VisualModels.FiguresBoard(Board,boardDefault)

createNewBoardFromLoad :: Board -> GuiMonad ()
createNewBoardFromLoad board = ask >>= \content -> getGState >>= \st -> do
    let model    = visualToModel board
        iconEdit = content ^. gSatIconEditBoard
        da       = content ^. gSatDrawArea 
    
    updateGState ((<~) gSatBoard board)
    updateGState ((<~) gSatModel model)
    
    io $ widgetHideAll iconEdit
    io $ widgetShowAll da
    return ()

-- | Crea un nuevo archivo en blanco.
createNewBoard :: GuiMonad ()
createNewBoard = createNewBoardFromLoad boardDefault

-- -- | Función para cargar un archivo.
-- openFile :: GuiMonad ()
-- openFile = ask >>= \ct -> get >>= \st ->
--            io $ dialogLoad "Cargar programa" funFileFilter (openFile' ct st) >>
--             return ()
--     where
--         openFile' :: GReader -> GStateRef -> Maybe TextFilePath ->
--                      Maybe String -> Maybe String -> IO ()
--         openFile' content st mfp mname mcode = 
--             evalRWST (createNewFileFromLoad mfp mname mcode) content st >> 
--             return ()
-- 
-- -- | Dialogo general para la carga de archivos.
-- dialogLoad :: String -> (FileChooserDialog -> IO ()) -> 
--               (Maybe TextFilePath -> Maybe String -> Maybe String -> IO ()) -> 
--               IO Bool
-- dialogLoad label fileFilter action = do
--     dialog <- fileChooserDialogNew (Just label) 
--                                     Nothing 
--                                     FileChooserActionOpen
--                                     [ ("Cargar",ResponseAccept)
--                                     , ("Cancelar",ResponseCancel)]
-- 
--     fileFilter dialog 
--     response <- dialogRun dialog
--     
--     case response of
--         ResponseAccept -> do
--             selected <- fileChooserGetFilename dialog
--             F.mapM_ (\filepath -> 
--                     readFile filepath >>= \code ->
--                     takeFileName filepath >>= \fileName ->
--                     action (Just $ pack filepath) (Just fileName) (Just code) >>
--                     widgetDestroy dialog) selected 
--             return True
--         _ -> widgetDestroy dialog >> return False
--     where
--         takeFileName :: FilePath -> IO String
--         takeFileName = return . takeBaseName
-- 
-- -- | Generador de filtros para la carga y guardado de archivos.
-- setFileFilter :: FileChooserClass f => f -> [String] -> String -> IO ()
-- setFileFilter fChooser patterns title = do
--                                 hsfilt <- fileFilterNew
--                                 mapM_ (fileFilterAddPattern hsfilt) patterns
--                                 fileFilterSetName hsfilt title
--                                 fileChooserAddFilter fChooser hsfilt    
-- 
-- -- | Guardado directo de un archivo.
-- saveFile :: GuiMonad ()
-- saveFile = getGState >>= \st ->
--         case st ^. gFunEditBook of
--             Nothing -> return ()
--             Just editBook -> do
--                 (mfp,_,textV) <- getTextEditFromFunEditBook editBook
--                 case mfp of
--                     Nothing -> saveAtFile
--                     Just fp -> getCode textV >>= save (unpack fp)
--     where
--         save:: FilePath -> String -> GuiMonad ()
--         save filepath code = io $ writeFile filepath code
--         getCode :: TextView -> GuiMonad String
--         getCode textV = io $ do
--             buf   <- textViewGetBuffer textV
--             start <- textBufferGetStartIter buf
--             end   <- textBufferGetEndIter buf
--             textBufferGetText buf start end False
-- 
-- -- | Guardado en, de un archivo.
-- saveAtFile :: GuiMonad ()
-- saveAtFile = getGState >>= \st ->
--         case st ^. gFunEditBook of
--             Nothing -> return ()
--             Just editBook -> do
--                 (_,name,textV) <- getTextEditFromFunEditBook editBook
--                 code <- getCode textV
--                 let nFile = if name == "blank" then "" else name
--                 mfp <- saveDialog "Guardar programa" (nFile++".fun") funFileFilter code
--                 when (isJust mfp) (updateFL editBook $ fromJust mfp)
--                 return ()
--     where
--         getCode :: TextView -> GuiMonad String
--         getCode textV = io $ do
--             buf   <- textViewGetBuffer textV
--             start <- textBufferGetStartIter buf
--             end   <- textBufferGetEndIter buf
--             textBufferGetText buf start end False
--         updateFL :: FunEditBook -> FilePath -> GuiMonad ()
--         updateFL editBook fp = do
--                 let ebook    = editBook ^. book
--                 let fileList = editBook ^. tabFileList
--                 cPageNum  <- io $ notebookGetCurrentPage ebook
--                 let updateFileList = upList fp cPageNum fileList
--                 updateGState ((<~) gFunEditBook (Just $ FunEditBook ebook updateFileList))
--         upList :: FilePath -> Int -> [Maybe TextFilePath] -> [Maybe TextFilePath]
--         upList fp n ls = (init $ take (n+1) ls) ++ [Just $ pack fp] ++ (drop (n+1) ls)
-- 
-- -- | Dialogo general para guardar un archivo.
-- saveDialog :: String -> String -> (FileChooserDialog -> IO ()) -> 
--               String -> GuiMonad (Maybe FilePath)
-- saveDialog label filename fileFilter serialItem = do
--         dialog <- io $ fileChooserDialogNew (Just label) 
--                                             Nothing 
--                                             FileChooserActionSave 
--                                             [ ("Guardar",ResponseAccept)
--                                             , ("Cancelar",ResponseCancel)
--                                             ]
--         
--         io $ fileChooserSetCurrentName dialog filename
--         io $ fileFilter dialog
--         response <- io $ dialogRun dialog
-- 
--         case response of
--             ResponseAccept -> io (fileChooserGetFilename dialog) >>= 
--                               \fp -> F.mapM_ save fp >> 
--                               io (widgetDestroy dialog) >> 
--                               return fp
--             _ -> io (widgetDestroy dialog) >> return Nothing
--     where
--         save:: FilePath -> GuiMonad ()
--         save filepath = io $ writeFile filepath serialItem
-- 
-- -- | Filtro de programas de fun.
-- funFileFilter :: (FileChooserClass f, MonadIO m) => f -> m ()
-- funFileFilter dialog = io $ setFileFilter dialog ["*.sat"] "Programa de sat"
