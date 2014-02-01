-- | Customised entry boxes for formulas.
{-# Language OverloadedStrings,DoAndIfThenElse #-}
module Sat.GUI.EntryFormula where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get,eventClick)

import Lens.Family

import Control.Monad.Trans.RWS hiding (state)
import Control.Monad
import Control.Arrow ((&&&))

import qualified Data.Foldable as F
import qualified Data.Map as M


import Data.Reference (readRef)
import Data.Text(unpack)
import qualified Data.Set as S

import Sat.VisualModel
import Sat.GUI.GState
import Sat.Core(eval,relations,predicates,pname,rname,isClosed,Signature)
import Sat.Parser(parseSignatureFormula,symbolList,getErrString)
import Sat.Signatures.Figures(figuras)


data FormulaState = Satisfied
                  | NSatisfied
                  | OpenFormula
                  | ParserError String
                  | Parsed
                  | NotChecked


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

initialFormString :: String
initialFormString = "Ingresar Fórmula"

entryFormAttrs :: [AttrOp CellRendererText]
entryFormAttrs = [ cellTextFont := "DejaVu Sans"
                 , cellTextEditable := True
                 ]

initFormStyle :: [AttrOp CellRendererText]
initFormStyle = [ cellTextFont := "DejaVu Sans"
                , cellTextStyle := StyleItalic
                , cellTextForeground := "Gray"
                , cellTextBackground := "White"
                ]
                
editedFormStyle :: [AttrOp CellRendererText]
editedFormStyle = [ cellTextFont := "DejaVu Sans"
                  , cellTextStyle := StyleNormal
                  , cellTextForeground := "Black"
                  , cellTextBackground := "White"
                  ]

setRowStyle :: FormulaItem -> [AttrOp CellRendererText]
setRowStyle row = txt:fmt
            where txt = cellText := fiName row
                  fmt = if fiName row == initialFormString
                        then initFormStyle
                        else editedFormStyle
                
initialFormula :: FormulaItem
initialFormula = FormulaItem initialFormString NotChecked
            
fListTOfiList :: [String] -> [FormulaItem]
fListTOfiList = map (flip FormulaItem NotChecked)

fiListTOfList :: [FormulaItem] -> [String]
fiListTOfList = map fiName

createNewEntryFormulaList :: [String] -> GuiMonad ()
createNewEntryFormulaList flist = getGState >>= \st -> do
    let sgn = signature (st ^. gSatBoard)
    configEntryFormula' $ map (parseFormulaItem sgn) flist
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
                set renderer entryFormAttrs >>
                
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
                                                      addSubMenu menu entry "Símbolo" symbolsMenu >>
                                                      addSubMenu menu entry "Predicado" predicatesMenu >>
                                                      addSubMenu menu entry "Relación" relationsMenu>>
                                                      widgetShowAll menu) >>
                                             return ()) >>


                                             
                onToolButtonClicked addb (addItem list' content stRef) >>
                                          
                -- Evento para borrar fórmula.
                onToolButtonClicked delb (delItem list' tv content stRef)  >>
                
                -- Evento para chequear las fórmulas.
                onToolButtonClicked checkb (evalGState content stRef $ 
                                                makeModelAndCheckFormula list' tv)>>
                
                cellLayoutPackStart colName renderer True >>
                cellLayoutSetAttributes colName renderer list' setRowStyle >>
                
                -- Icono de estado de fórmula:
                cellRendererPixbufNew >>= \pix ->
                cellLayoutPackStart colName pix False >>
                
                treeViewColumnSetSizing colName TreeViewColumnAutosize >>
                                                    
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

addSubMenu :: (MenuClass submenu, MenuClass menu) => menu -> Entry -> String -> (Entry -> IO submenu)  -> IO ()
addSubMenu menu entry title mkEntries = mkEntries entry >>= \entries ->
                                        menuItemNewWithLabel title >>= \item -> 
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

parseFormulaItem :: Signature -> String -> FormulaItem
parseFormulaItem sgn strForm = FormulaItem strForm $ 
                               either (ParserError . getErrString) (const Parsed) $ 
                               parseSignatureFormula sgn strForm 

updateFormula :: GStateRef -> ListStore FormulaItem -> String -> TreeIter -> IO ()
updateFormula stRef list strForm ti = readRef stRef >>= \st -> do
                                      let sgn = signature (st ^. gSatBoard)
                                      listStoreSetValue list 
                                                        (listStoreIterToIndex ti) 
                                                        (parseFormulaItem sgn strForm)

checkFormula :: GStateRef -> ListStore FormulaItem -> TreeIter -> IO Bool
checkFormula gsr store ti = 
    readRef gsr >>= 
    return . (signature . (^. gSatBoard) &&& (^. gSatModel)) >>= \(sgn,model) ->
    return (listStoreIterToIndex ti) >>= \ind ->
    listStoreGetValue store ind >>= \fi ->
    listStoreSetValue store ind (fi { fiState = state sgn model (fiName fi) }) >> 
    return False
    where state sgn model strForm = either (ParserError . getErrString) (check model) $ 
                                     parseSignatureFormula sgn strForm 
          check model formula = if not (isClosed formula)
                                then OpenFormula
                                else if eval formula model M.empty
                                     then Satisfied
                                     else NSatisfied
   

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
