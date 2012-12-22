{-# Language OverloadedStrings #-}
module Sat.GUI.EntryFormula where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get,eventClick)
import Graphics.UI.Gtk.Gdk.Events

import Text.Parsec.Error (ParseError)

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
import qualified Data.Set as S

import Sat.VisualModel
import Sat.GUI.GState
import Sat.Core(eval,relations,predicates,Predicate(..),Relation(..),Formula)
import Sat.Parser(parseSignatureFormula,symbolList)
import Sat.Signatures.Figures(figuras)


data FormulaState = Satisfied   | NSatisfied
                  | NotChecked 
                  | ParserError | Parsed

fiSafisfied, fiNSafisfied, fiNotChecked, fiParsed :: String
fiSafisfied  = "Fórmula satisfactible."
fiParsed     = "Fórmula parseada satisfactoriamente."
fiNSafisfied = "Fórmula no satisfactible."
fiNotChecked = "Fórmula no chequeada."

fStateIcon :: FormulaState -> StockId
fStateIcon Satisfied   = stockApply
fStateIcon NSatisfied  = stockCancel
fStateIcon NotChecked  = ""
fStateIcon Parsed  = stockHelp
fStateIcon ParserError = stockCapsLockWarning

data FormulaItem = FormulaItem { fiName  :: String
                               , fiState :: FormulaState
                               , fiInfo  :: String
                               }

initialFormulaList :: GuiMonad [FormulaItem]
initialFormulaList = io $ do
            entry <- return "Ingresar Fórmula."
            return $ [(FormulaItem entry NotChecked fiNotChecked)]
            
fListTOfiList :: [String] -> [FormulaItem]
fListTOfiList = map (flip (flip FormulaItem NotChecked) fiNotChecked)

fiListTOfList :: [FormulaItem] -> [String]
fiListTOfList = map fiName

createNewEntryFormulaList :: [String] -> GuiMonad ()
createNewEntryFormulaList flist = getGState >>= \st -> do
    configEntryFormula' $ map (parseFormulaItem st) flist
    updateGState ((<~) gSatFList flist)

createNewEntryFormula :: GuiMonad ()
createNewEntryFormula = configEntryFormula []

configEntryFormula :: [FormulaItem] -> GuiMonad ()
configEntryFormula list = do
    initf <- initialFormulaList
    configEntryFormula' (list++initf)
    updateGState ((<~) gSatFList (fiListTOfList initf))

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
            let boxTV  = content ^. (gSatTVFormula . gBoxTreeView)
                addb   = content ^. (gSatTVFormula . gAddFButton)
                delb   = content ^. (gSatTVFormula . gDelFButton)
                checkb = content ^. (gSatTVFormula . gCheckFButton)
                infoSb = content ^. gSatInfoStatusbar
            io $
                treeViewNewWithModel list >>= \tv ->
                cleanBoxTV boxTV >>
                containerAdd boxTV tv >>
                widgetShowAll boxTV >>
                treeViewGetColumn tv 0 >>=
                F.mapM_ (treeViewRemoveColumn tv) >>
                
                treeViewGetSelection tv >>= \seltv ->
                treeSelectionSetMode seltv SelectionSingle >>
                
                treeViewColumnNew >>= \colName ->
                treeViewSetHeadersVisible tv False >>
                treeViewSetModel tv list >>
                
                cellRendererTextNew >>= \renderer ->
                set renderer [ cellTextEditable := True ] >>
                
                on tv cursorChanged (treeViewGetSelection tv >>= 
                                     treeSelectionGetSelected >>=
                                     updateStatusBar infoSb list
                                    ) >>
                
                on renderer edited (\tp s -> treeModelGetIter list tp >>= \(Just ti) ->
                                             updateFormula stRef list s ti >>
                                             updateFList content stRef list >>
                                             evalGState content stRef addToUndo >> 
                                             return ()) >>

                on renderer editingStarted (\w tp -> treeModelGetIter list tp >>= \(Just ti) ->
                                             return (listStoreIterToIndex ti) >>= \ind ->
                                             return (castToEntry w) >>= \entry ->
                                             on entry entryPopulatePopup 
                                                      (\menu ->
                                                      symbolsMenu entry >>= \symbolsmenu ->
                                                      predicatesMenu entry >>= \predsmenu ->
                                                      relationsMenu entry >>= \relsmenu ->
                                                      menuItemNewWithLabel "Símbolo" >>= \mitemS ->
                                                      menuItemNewWithLabel "Predicado" >>= \mitemP ->
                                                      menuItemNewWithLabel "Relación" >>= \mitemR ->
                                                      menuItemSetSubmenu mitemS symbolsmenu >>
                                                      menuItemSetSubmenu mitemP predsmenu >>
                                                      menuItemSetSubmenu mitemR relsmenu >>
                                                      containerAdd menu mitemS >> 
                                                      containerAdd menu mitemP >> 
                                                      containerAdd menu mitemR >> 
                                                      widgetShowAll menu) >>
                                             return ()) >>
                                             
                onToolButtonClicked addb (listStoreAppend list (FormulaItem ("Ingresar Fórmula.") NotChecked fiNotChecked) >>
                                          updateFList content stRef list >>
                                          evalGState content stRef addToUndo >> 
                                          return ()) >>
                                          
                -- Evento para borrar fórmula.
                onToolButtonClicked delb (listStoreDeleteSelcts list tv >>
                                          updateFList content stRef list >>
                                          evalGState content stRef addToUndo >>
                                          getLastElemTreeView list >>=
                                          maybe (return ()) (selectLast tv)
                                          ) >>
                
                -- Evento para chequear las fórmulas.
                onToolButtonClicked checkb (evalGState content stRef $ 
                                                makeModelAndCheckFormula list)>>
                
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
                treeViewAppendColumn tv colName >>
                return ()
            return ()
        
        selectLast :: TreeView -> TreeIter -> IO ()
        selectLast tv iter = treeViewGetSelection tv >>= \sel ->
                             treeSelectionSelectIter sel iter
        
        makeModelAndCheckFormula :: ListStore FormulaItem -> GuiMonad ()
        makeModelAndCheckFormula list = get >>= \stRef -> getGState >>= \st -> do
            let visual = st ^. gSatBoard
                model  = visualToModel visual
                
            updateGState ((<~) gSatModel model)
            io $ treeModelForeach list (checkFormula stRef list)

        updateStatusBar :: Statusbar -> ListStore FormulaItem -> 
                           Maybe TreeIter -> IO ()
        updateStatusBar _ _ Nothing = return ()
        updateStatusBar infoSb list (Just iter) = do
                        let i = listStoreIterToIndex iter
                        
                        ctx <- statusbarGetContextId infoSb "Line"
                        fi <- listStoreGetValue list i
                        statusbarPop infoSb ctx
                        statusbarPush infoSb ctx (fiInfo fi)
                        return ()
        cleanBoxTV :: ScrolledWindow -> IO ()
        cleanBoxTV sw = do
                chldr <- containerGetChildren sw 
                mapM_ (containerRemove sw) chldr
         
getLastElemTreeView :: ListStore FormulaItem -> IO (Maybe TreeIter)
getLastElemTreeView ls = treeModelGetIterFirst ls >>= \fiter ->
                         maybe (return fiter) (getLastElemTreeView' ls) fiter
    where
        getLastElemTreeView' :: ListStore FormulaItem -> TreeIter -> IO (Maybe TreeIter)
        getLastElemTreeView' ls iter =
                        treeModelIterNext ls iter >>= \niter ->
                        maybe (return $ Just iter) (getLastElemTreeView' ls) niter
         
updateFList :: GReader -> GStateRef -> ListStore FormulaItem -> IO ()
updateFList content stRef list = do
    fil <- listStoreToList list
    evalGState content stRef (updateGState ((<~) gSatFList (fiListTOfList fil)))

parseFormulaItem :: GState -> String -> FormulaItem
parseFormulaItem st strForm = 
    case parseSignatureFormula (signature (st ^. gSatBoard)) strForm of
        Left er -> FormulaItem strForm ParserError (prettyPrintErr er)
        Right _ -> FormulaItem strForm Parsed fiParsed
     where
        prettyPrintErr :: ParseError -> String
        prettyPrintErr = show

updateFormula :: GStateRef -> ListStore FormulaItem -> String -> TreeIter -> IO ()
updateFormula stRef list strForm ti = readRef stRef >>= \st -> do
    let ind = listStoreIterToIndex ti
    case parseSignatureFormula (signature (st ^. gSatBoard)) strForm of
        Left er -> let fi = FormulaItem strForm ParserError (prettyPrintErr er)
                   in listStoreSetValue list ind fi
        Right _ -> let fi = FormulaItem strForm Parsed fiParsed
                   in listStoreSetValue list ind fi
     where
        prettyPrintErr :: ParseError -> String
        prettyPrintErr = show

checkFormula :: GStateRef -> ListStore FormulaItem -> TreeIter -> IO Bool
checkFormula gsr store ti =
    readRef gsr >>= \st ->
    return (listStoreIterToIndex ti) >>= \ind ->
    listStoreGetValue store ind >>= \fi@(FormulaItem strForm _ _) ->
    case parseSignatureFormula (signature (st ^. gSatBoard)) strForm of
        Right formula -> check fi ind formula
        Left er -> listStoreSetValue store ind (fi { fiState = ParserError
                                                   , fiInfo = (prettyPrintErr er)
                                                   } ) >> return False
    where
        check :: FormulaItem -> Int -> Formula -> IO Bool
        check fi ind formula = do
            gstate <- readIORef gsr
            let model = gstate ^. gSatModel
                verified = eval formula model M.empty
            if verified
                then listStoreSetValue store ind (fi { fiState = Satisfied
                                                     , fiInfo = fiSafisfied
                                                     }) >> return False
                else listStoreSetValue store ind (fi { fiState = NSatisfied
                                                     , fiInfo = fiNSafisfied
                                                     } ) >> return False
        prettyPrintErr :: ParseError -> String
        prettyPrintErr = show
   
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
                           listStoreSetValue list ind (FormulaItem str NotChecked fiNotChecked) >>
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
    
    
relationsMenu :: Entry -> IO Menu
relationsMenu entry = do
    let rels = S.toList $ S.map rname (relations figuras)
    menu <- predsRelsMenu entry rels
    
    return menu
    
    
predicatesMenu :: Entry -> IO Menu
predicatesMenu entry = do
    let preds = S.toList $ S.map pname (predicates figuras)
    menu <- predsRelsMenu entry preds
    
    return menu
    
   
   
    
predsRelsMenu :: Entry -> [String] -> IO Menu
predsRelsMenu entry names = do
    menu <- menuNew
    forM_ names (\n -> menuItemNewWithLabel n >>= \item ->
                   on item menuItemActivate
                        (editableGetPosition entry >>=
                         editableInsertText entry (n ++ "()") >> return ()) >>
                      containerAdd menu item >>
                      return ())
    
    return menu
