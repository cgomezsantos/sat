-- | Customised entry boxes for formulas.
{-# Language OverloadedStrings,DoAndIfThenElse #-}
module Sat.GUI.EntryFormula where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get,eventClick)

import Text.Parsec.Error (ParseError)

import Lens.Family

import Control.Monad.Trans.RWS
import Control.Monad

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.IORef(readIORef)

import Data.Reference (readRef)
import Data.Text(unpack)
import qualified Data.Set as S

import Sat.VisualModel
import Sat.GUI.GState
import Sat.Core(eval,relations,predicates,Formula,pname,rname,isClosed)
import Sat.Parser(parseSignatureFormula,symbolList,getErrString)
import Sat.Signatures.Figures(figuras)


data FormulaState = Satisfied   
                  | NSatisfied
                  | OpenFormula
                  | NotChecked 
                  | ParserError String
                  | Parsed

instance Show FormulaState where
         show Satisfied  = "Fórmula satisfactible."
         show Parsed     = "Fórmula parseada satisfactoriamente."
         show NSatisfied = "Fórmula no satisfactible."
         show NotChecked = "Fórmula no chequeada."
         show (ParserError err) = err
         show OpenFormula = "Fórmula con variables libres"

fStateIcon :: FormulaState -> StockId
fStateIcon Satisfied   = stockApply
fStateIcon NSatisfied  = stockCancel
fStateIcon NotChecked  = stockDialogQuestion
fStateIcon Parsed  = stockOk
fStateIcon (ParserError _) = stockDialogError
fStateIcon OpenFormula = stockDialogError

data FormulaItem = FormulaItem { fiName  :: String
                               , fiState :: FormulaState                               
                               }

initFormStyle = [ cellTextFont := "DejaVu Sans"
                , cellTextStyle := StyleItalic
                , cellTextForeground := "Gray"
                , cellTextEditable := True
                ]

editedFormStyle = [ cellTextStyle := StyleNormal
                  , cellTextForeground := "Black"
                  ]

                
initialFormula :: FormulaItem
initialFormula = FormulaItem "Ingresar Fórmula" NotChecked
            
fListTOfiList :: [String] -> [FormulaItem]
fListTOfiList = map (flip FormulaItem NotChecked)

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
    configEntryFormula' (list++[initialFormula])
    updateGState ((<~) gSatFList [fiName initialFormula])

configEntryFormula' :: [FormulaItem] -> GuiMonad ()
configEntryFormula' list = do
    ls <- io $ listStoreNew list
    setupEFList ls
    where
        listStoreDeleteSelcts :: ListStore FormulaItem -> TreeView -> IO ()
        listStoreDeleteSelcts list' tv = do
                seltv  <- treeViewGetSelection tv
                miter  <- treeSelectionGetSelected seltv
                case miter of
                    Nothing -> return ()
                    Just iter -> do 
                            let ind = listStoreIterToIndex iter
                            listStoreRemove list' ind

        addItem lst content stRef = listStoreAppend lst initialFormula >>
                                    updateFList content stRef lst >>
                                    evalGState content stRef addToUndo >> 
                                    return ()
                                    
        delItem lst tv content stRef = listStoreDeleteSelcts lst tv >>
                                       updateFList content stRef lst >>
                                       evalGState content stRef addToUndo >>
                                       getLastElemTreeView lst >>=
                                       maybe (return ()) (selectLast tv) 
        
        setupEFList :: ListStore FormulaItem -> GuiMonad ()
        setupEFList list' = do
            content <- ask
            stRef <- get
            let boxTV  = content ^. (gSatTVFormula . gBoxTreeView)
                addb   = content ^. (gSatTVFormula . gAddFButton)
                delb   = content ^. (gSatTVFormula . gDelFButton)
                checkb = content ^. (gSatTVFormula . gCheckFButton)
                infoSb = content ^. gSatInfoStatusbar
            io $
                treeViewNewWithModel list' >>= \tv ->
                cleanBoxTV boxTV >>
                containerAdd boxTV tv >>
                widgetShowAll boxTV >>
                treeViewGetColumn tv 0 >>=
                F.mapM_ (treeViewRemoveColumn tv) >>
                
                treeViewGetSelection tv >>= \seltv ->
                treeSelectionSetMode seltv SelectionSingle >>
                
                treeViewColumnNew >>= \colName ->
                treeViewSetHeadersVisible tv False >>
                treeViewSetModel tv list' >>
                
                cellRendererTextNew >>= \renderer ->
                set renderer initFormStyle >>
                
                on tv cursorChanged (updateStatus infoSb list' tv) >>
                
                on renderer edited (\tp s -> treeModelGetIter list' tp >>= \(Just ti) ->
                                             updateFormula stRef list' s ti >>
                                             updateFList content stRef list' >>
                                             evalGState content stRef addToUndo >> 
                                             updateStatus infoSb list' tv >>
                                             set renderer editedFormStyle >>
                                             return ()) >>
                                             
                after tv keyPressEvent (do
                   key <- eventKeyName
                   let actions = [ ("Insert", addItem list' content stRef)
                                 , ("Delete", delItem list' tv content stRef)
                                 ]
                   maybe (return False) (\act -> io act >> return True) (lookup key actions)
                ) >>
                
                on renderer editingStarted (\w _ -> 
                                             return (castToEntry w) >>= \entry ->
                                             on entry entryPopulatePopup 
                                                      (\menu ->
                                                      symbolsMenu entry >>= \symbolsmenu ->
                                                      predicatesMenu entry >>= \predsmenu ->
                                                      relationsMenu entry >>= \relsmenu ->
                                                      addSubMenu menu "Símbolo" symbolsmenu >>
                                                      addSubMenu menu "Predicado" predsmenu >>
                                                      addSubMenu menu "Relación" relsmenu >>
                                                      widgetShowAll menu) >>
                                             return ()) >>


                                             
                onToolButtonClicked addb (addItem list' content stRef) >>
                                          
                -- Evento para borrar fórmula.
                onToolButtonClicked delb (delItem list' tv content stRef)  >>
                
                -- Evento para chequear las fórmulas.
                onToolButtonClicked checkb (evalGState content stRef $ 
                                                makeModelAndCheckFormula list' tv)>>
                
                cellLayoutPackStart colName renderer True >>
                
                -- Icono de estado de fórmula:
                cellRendererPixbufNew >>= \pix ->
                set pix [ cellPixbufStockId := fStateIcon NotChecked ] >>
                cellLayoutPackStart colName pix False >>
                
                treeViewColumnSetSizing colName TreeViewColumnAutosize >>
                
                cellLayoutSetAttributes colName renderer list'
                                    (\ind -> [ cellText := fiName ind ]) >>
                                    
                cellLayoutSetAttributes colName pix list' 
                                    (\ind -> [ cellPixbufStockId := fStateIcon $ fiState ind ]) >>
                treeViewAppendColumn tv colName >>
                return ()
            return ()


        
        selectLast :: TreeView -> TreeIter -> IO ()
        selectLast tv iter = treeViewGetSelection tv >>= \sel ->
                             treeSelectionSelectIter sel iter
        
        makeModelAndCheckFormula :: ListStore FormulaItem -> TreeView -> GuiMonad ()
        makeModelAndCheckFormula list' tv = get >>= \stRef -> getGState >>= \st -> 
                                            ask >>= \content -> do
            let visual = st ^. gSatBoard
                model  = visualToModel visual
                infoSb = content ^. gSatInfoStatusbar
            updateGState ((<~) gSatModel model)
            io $ treeModelForeach list' (checkFormula stRef list')
            io $ updateStatus infoSb list' tv 

        updateStatus infoSb list' tv = treeViewGetSelection tv >>= 
                                       treeSelectionGetSelected >>=
                                       updateStatusBar infoSb list'


        updateStatusBar :: Statusbar -> ListStore FormulaItem -> 
                           Maybe TreeIter -> IO ()
        updateStatusBar infoSb _ Nothing = do
                        ctx <- statusbarGetContextId infoSb "Line"
                        _ <- statusbarPop infoSb ctx
                        _ <- statusbarPush infoSb ctx 
                                "Elija una fórmula para ver su condición"
                        return ()
        updateStatusBar infoSb list' (Just iter) = do
                        let i = listStoreIterToIndex iter
                        
                        ctx <- statusbarGetContextId infoSb "Line"
                        fi <- listStoreGetValue list' i
                        _ <- statusbarPop infoSb ctx
                        _ <- statusbarPush infoSb ctx (show $ fiState fi)
                        return ()
        cleanBoxTV :: ScrolledWindow -> IO ()
        cleanBoxTV sw = do
                chldr <- containerGetChildren sw 
                mapM_ (containerRemove sw) chldr

addSubMenu :: (MenuClass submenu, MenuClass menu) => menu -> String -> submenu -> IO ()
addSubMenu menu title entries = menuItemNewWithLabel title >>= \item -> 
                                menuItemSetSubmenu item entries >>
                                containerAdd menu item

         
getLastElemTreeView :: ListStore FormulaItem -> IO (Maybe TreeIter)
getLastElemTreeView ls = treeModelGetIterFirst ls >>= \fiter ->
                         maybe (return fiter) (getLastElemTreeView' ls) fiter
    where
        getLastElemTreeView' :: ListStore FormulaItem -> TreeIter -> IO (Maybe TreeIter)
        getLastElemTreeView' ls' iter =
                        treeModelIterNext ls iter >>= 
                        maybe (return $ Just iter) (getLastElemTreeView' ls') 
         
updateFList :: GReader -> GStateRef -> ListStore FormulaItem -> IO ()
updateFList content stRef list = do
    fil <- listStoreToList list
    evalGState content stRef (updateGState ((<~) gSatFList (fiListTOfList fil)))

parseFormulaItem :: GState -> String -> FormulaItem
parseFormulaItem st strForm = 
    case parseSignatureFormula (signature (st ^. gSatBoard)) strForm of
        Left er -> FormulaItem strForm $ ParserError (prettyPrintErr er)
        Right _ -> FormulaItem strForm Parsed 
     where
        prettyPrintErr :: ParseError -> String
        prettyPrintErr = getErrString

updateFormula :: GStateRef -> ListStore FormulaItem -> String -> TreeIter -> IO ()
updateFormula stRef list strForm ti = readRef stRef >>= \st -> do
    let ind = listStoreIterToIndex ti
    case parseSignatureFormula (signature (st ^. gSatBoard)) strForm of
        Left er -> let fi = FormulaItem strForm $ ParserError (prettyPrintErr er)
                   in listStoreSetValue list ind fi
        Right _ -> let fi = FormulaItem strForm Parsed 
                   in listStoreSetValue list ind fi
     where
        prettyPrintErr :: ParseError -> String
        prettyPrintErr = getErrString

checkFormula :: GStateRef -> ListStore FormulaItem -> TreeIter -> IO Bool
checkFormula gsr store ti =
    readRef gsr >>= \st ->
    return (listStoreIterToIndex ti) >>= \ind ->
    listStoreGetValue store ind >>= \fi@(FormulaItem strForm _) ->
    case parseSignatureFormula (signature (st ^. gSatBoard)) strForm of
        Right formula -> check fi ind formula
        Left er -> listStoreSetValue store ind (fi { fiState = ParserError (prettyPrintErr er)
                                                   } ) >> return False
    where
        check :: FormulaItem -> Int -> Formula -> IO Bool
        check fi ind formula = do
            gstate <- readIORef gsr
            let model = gstate ^. gSatModel
            if not (isClosed formula)
            then listStoreSetValue store ind (fi { fiState = OpenFormula }) >> return False
            else if eval formula model M.empty
                then listStoreSetValue store ind (fi { fiState = Satisfied }) >> return False
                else listStoreSetValue store ind (fi { fiState = NSatisfied } ) >> return False
        prettyPrintErr :: ParseError -> String
        prettyPrintErr = getErrString
   

symbolsMenu :: Entry -> IO Menu
symbolsMenu entry = do
    menu <- menuNew
    forM_ symbolList $ \s -> menuItemNewWithLabel (unpack s) >>= \item ->
                      on item menuItemActivate 
                        (editableGetPosition entry >>=
                         editableInsertText entry (unpack s) >> return ()) >>
                      containerAdd menu item >>
                      return ()
    
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
                         editableInsertText entry n >> return ()) >>
                      containerAdd menu item >>
                      return ())
    
    return menu
