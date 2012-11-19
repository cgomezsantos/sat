{-# Language OverloadedStrings #-}
module Sat.GUI.EntryFormula where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get,eventClick)
import Graphics.UI.Gtk.Gdk.Events

import Lens.Family

import Data.Tree

import Control.Monad.Trans.RWS
import Control.Monad

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.IORef(readIORef)
import Data.Maybe(fromJust)

import Data.Reference (readRef)
import Data.Text(unpack)

import Sat.VisualModel
import Sat.GUI.GState
import Sat.Core(eval)
import Sat.Parser(parseSignatureFormula,symbolList)

data FormulaState = Satisfied | NSatisfied | NotChecked | ParserError

fStateIcon :: FormulaState -> StockId
fStateIcon Satisfied = stockApply
fStateIcon NSatisfied = stockCancel
fStateIcon NotChecked = stockCapsLockWarning
fStateIcon ParserError = stockNo

data FormulaItem = FormulaItem { fiName  :: String
                               , fiState :: FormulaState 
                               }

initialFormulaList :: GuiMonad [FormulaItem]
initialFormulaList = io $ do
            entry <- return "Ingresar Fórmula."
            return $ [(FormulaItem entry NotChecked)]
            
fListTOfiList :: [String] -> [FormulaItem]
fListTOfiList = map (flip FormulaItem NotChecked)

fiListTOfList :: [FormulaItem] -> [String]
fiListTOfList = map fiName

createNewEntryFormulaList :: [String] -> GuiMonad ()
createNewEntryFormulaList flist = do
    configEntryFormula' $ fListTOfiList flist
    updateGState ((<~) gSatFList flist)

configEntryFormula :: [FormulaItem] -> GuiMonad ()
configEntryFormula list = do
    initf <- initialFormulaList
    configEntryFormula' (list++initf)

configEntryFormula' :: [FormulaItem] -> GuiMonad ()
configEntryFormula' list = do
    ls <- io $ listStoreNew list
    setupEFList ls
    where
        listStoreDeleteSelcts :: ListStore FormulaItem -> TreeView -> IO ()
        listStoreDeleteSelcts list tv = do
                seltv  <- treeViewGetSelection tv
                miter  <- treeSelectionGetSelected seltv
                case miter of
                    Nothing -> return ()
                    Just iter -> do 
                            let ind = listStoreIterToIndex iter
                            listStoreRemove list ind
        
        setupEFList :: ListStore FormulaItem -> GuiMonad ()
        setupEFList list = do
            content <- ask
            stRef <- get
            let tv     = content ^. (gSatTVFormula . gTreeView)
                addb   = content ^. (gSatTVFormula . gAddFButton)
                delb   = content ^. (gSatTVFormula . gDelFButton)
                checkb = content ^. (gSatTVFormula . gCheckFButton)
            io $               
                treeViewGetColumn tv 0 >>=
                F.mapM_ (treeViewRemoveColumn tv) >>
                
                treeViewGetSelection tv >>= \seltv ->
                treeSelectionSetMode seltv SelectionSingle >>
                
                treeViewColumnNew >>= \colName ->
                treeViewSetHeadersVisible tv False >>
                treeViewSetModel tv list >>
                
                cellRendererTextNew >>= \renderer ->
                set renderer [ cellTextEditable := True
                             ] >>
                
                on renderer edited (\tp s -> treeModelGetIter list tp >>= \(Just ti) ->
                                             return (listStoreIterToIndex ti) >>= \ind ->
                                             listStoreSetValue list ind (FormulaItem s NotChecked) >> 
                                             updateFList content stRef list >>
                                             return ()) >>

                on renderer editingStarted (\w tp -> treeModelGetIter list tp >>= \(Just ti) ->
                                             return (listStoreIterToIndex ti) >>= \ind ->
                                             return (castToEntry w) >>= \entry ->
                                             on entry entryPopulatePopup 
                                                      (\menu ->
                                                      symbolsMenu entry >>= \symbolsmenu ->
                                                      menuItemNewWithLabel "Símbolo" >>= \mitem ->
                                                      menuItemSetSubmenu mitem symbolsmenu >>
                                                      containerAdd menu mitem >> widgetShowAll menu) >>
                                             return ()) >>
                                             
                onToolButtonClicked addb (listStoreAppend list (FormulaItem ("Ingresar Fórmula.") NotChecked ) >>
                                          updateFList content stRef list >>
                                          return ()) >>
                                          
                -- Evento para borrar fórmula.
                onToolButtonClicked delb (listStoreDeleteSelcts list tv >>
                                          updateFList content stRef list) >>
                
                -- Evento para chequear las fórmulas.
                onToolButtonClicked checkb (treeModelForeach list (checkFormula stRef list)) >>
                
                cellLayoutPackStart colName renderer True >>
                
                -- Icono de estado de fórmula:
                cellRendererPixbufNew >>= \pix ->
                set pix [ cellPixbufStockId := fStateIcon NotChecked ] >>
                cellLayoutPackStart colName pix False >>
                
                treeViewColumnSetSizing colName TreeViewColumnAutosize >>
                
                cellLayoutSetAttributes colName renderer list 
                                    (\ind -> [ cellText := fiName ind ]) >>
                                    
                cellLayoutSetAttributes colName pix list 
                                    (\ind -> [ cellPixbufStockId := fStateIcon $ fiState ind ]) >>
                                    
                treeViewAppendColumn tv colName >>= \ncolumns ->
                putStrLn ("agregada columna"++ show ncolumns) >>
                return ()
            return ()

updateFList :: GReader -> GStateRef -> ListStore FormulaItem -> IO ()
updateFList content stRef list = do
    fil <- listStoreToList list
    evalGState content stRef (updateGState ((<~) gSatFList (fiListTOfList fil)))
            

checkFormula :: GStateRef -> ListStore FormulaItem -> TreeIter -> IO Bool
checkFormula gsr store ti =
    readRef gsr >>= \st ->
    return (listStoreIterToIndex ti) >>= \ind ->
    listStoreGetValue store ind >>= \fi@(FormulaItem strform state) ->
    case (parseSignatureFormula (signature (st ^. gSatBoard)) strform) of
         Left er -> putStrLn ("Error parseando "++strform) >>
                    listStoreSetValue store ind (fi { fiState = ParserError } ) >>
                    return False
         Right formula ->
            do
                gstate <- readIORef gsr
                let model = gstate ^. gSatModel
                putStrLn $ "Evaluando fórmula "++ show formula
                verified <- return $ eval formula model M.empty
                if verified
                    then listStoreSetValue store ind (fi { fiState = Satisfied }) >>
                         return False
                    else listStoreSetValue store ind (fi { fiState = NSatisfied} ) >>
                         return False
   

   
dialogEntryFormula :: ListStore FormulaItem -> Int -> CellRendererText -> IO ()
dialogEntryFormula list ind cellr =
    dialogNew >>= \dialog ->
    windowSetPosition dialog WinPosMouse >>
    set dialog [ windowDecorated := False
               , windowModal := True
               ] >>
    dialogGetUpper dialog >>= \vb ->
    entryNew >>= \entry ->
    boxPackStart vb entry PackNatural 0 >>
    widgetShowAll dialog >>
    onEntryActivate entry (entryGetText entry >>= \str ->
                           set cellr [ cellText := str ] >>
                           listStoreSetValue list ind (FormulaItem str NotChecked) >>
                           dialogResponse dialog ResponseOk >>
                           widgetDestroy dialog) >>
                           
    dialogRun dialog >>
    cellRendererStopEditing cellr True >>
    return ()
    


symbolsMenu :: Entry -> IO Menu
symbolsMenu entry = do
    menu <- menuNew
    forM_ symbolList (\s -> menuItemNewWithLabel (unpack s) >>= \item ->
                      on item menuItemActivate 
                        (editableGetPosition entry >>=
                         editableInsertText entry (unpack s) >> return ()) >>
                      containerAdd menu item >>
                      return ())
    
    -- Hay que setear eventos para cada item, de manera que se pegue
    -- el símbolo en el entry
    return menu
   
   
    