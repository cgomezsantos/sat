-- | Customised entry boxes for formulas.
{-# Language OverloadedStrings,DoAndIfThenElse #-}
module Sat.GUI.EntryFormula where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get,eventClick)

import Control.Monad
import Control.Lens hiding (set,act)

import Control.Monad.Trans.RWS hiding (state)
import Control.Arrow ((&&&))

import qualified Data.Map as M

import Data.Text(unpack,pack,Text)
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
         show Satisfied  = "La fórmula se satisface en el modelo."
         show Parsed     = "La fórmula fue parseada satisfactoriamente."
         show NSatisfied = "La fórmula no se satisfe en el modelo."
         show NotChecked = "Fórmula no chequeada."
         show (ParserError err) = err
         show OpenFormula = "La fórmula tiene variables libres."

fStateIcon :: FormulaState -> StockId
fStateIcon Satisfied   = stockApply
fStateIcon NSatisfied  = stockCancel
fStateIcon NotChecked  = stockDialogQuestion
fStateIcon Parsed  = stockOk
fStateIcon (ParserError _) = stockDialogError
fStateIcon OpenFormula = stockDialogError

fStateText :: FormulaState -> Text
fStateText Satisfied   = "T "
fStateText NSatisfied  = "F "
fStateText NotChecked  = "  "
fStateText Parsed  = "Ok"
fStateText (ParserError _) = "? "
fStateText OpenFormula = "?"

data FormulaItem = FormulaItem { fiName  :: String
                               , fiState :: FormulaState
                               }

statusTextStyle :: [AttrOp CellRendererText]
statusTextStyle = [ cellTextWeight := 800
                  , cellTextWeightSet := True
                  , cellTextForeground := ("Black" :: Text)
                  , cellTextBackground := ("#eeeeee" :: Text)
                  ]


finame :: Lens' FormulaItem String
finame = lens fiName (\fi str -> fi {fiName = str})

fistate :: Lens' FormulaItem FormulaState
fistate = lens fiState (\fi str -> fi {fiState = str})


initialFormString :: Text
initialFormString = "Ingresar Fórmula"

entryFormAttrs :: [AttrOp CellRendererText]
entryFormAttrs = [ cellTextFont := ("DejaVu Sans" :: Text)
                 , cellTextEditable := True
                 ]

initFormStyle :: [AttrOp CellRendererText]
initFormStyle = [ cellTextFont := ("DejaVu Sans" :: Text)
                , cellTextStyle := StyleItalic
                , cellTextForeground := ("#666666" :: Text)
                , cellTextBackground := ("White" :: Text)
                ]
                
editedFormStyle :: [AttrOp CellRendererText]
editedFormStyle = [ cellTextFont := ("DejaVu Sans" :: Text)
                  , cellTextStyle := StyleNormal
                  , cellTextForeground := ("Black" :: Text)
                  , cellTextBackground := ("White" :: Text)
                  ]

setRowStyle :: FormulaItem -> [AttrOp CellRendererText]
setRowStyle row = txt:fmt
            where txt = cellText := fiName row
                  fmt = if pack (fiName row) == initialFormString
                        then initFormStyle
                        else editedFormStyle
                
initialFormula :: FormulaItem
initialFormula = FormulaItem (unpack initialFormString) NotChecked
            
fListTOfiList :: [String] -> [FormulaItem]
fListTOfiList = map (flip FormulaItem NotChecked)

fiListTOfList :: [FormulaItem] -> [String]
fiListTOfList = map fiName

createNewEntryFormulaList :: [String] -> GuiMonad ()
createNewEntryFormulaList flist = useG (gSatBoard . to signature ) >>= \sgn ->  
                          configEntryFormula' (map (parseFormulaItem sgn) flist) >>
                          updateStateField gSatFList flist 

createNewEntryFormula :: GuiMonad ()
createNewEntryFormula = configEntryFormula []

configEntryFormula :: [FormulaItem] -> GuiMonad ()
configEntryFormula list = do
    configEntryFormula' (list++[initialFormula])
    updateStateField  gSatFList [initialFormula ^. finame]

configEntryFormula' :: [FormulaItem] -> GuiMonad ()
configEntryFormula' = io . listStoreNew >=> setupEFList
    where
        lsDelSel :: ListStore FormulaItem -> TreeView -> IO ()
        lsDelSel list tv = treeViewGetSelection tv >>= 
                            treeSelectionGetSelected >>=
                            maybe (return ()) 
                                  (listStoreRemove list . listStoreIterToIndex)

        addItem lst content stRef = listStoreAppend lst initialFormula >>
                                    updateFList content stRef lst >>
                                    evalGState content stRef addToUndo >> 
                                    return ()
                                    
        delItem lst tv content stRef = lsDelSel lst tv >>
                                       updateFList content stRef lst >>
                                       evalGState content stRef addToUndo >>
                                       getLastElemTreeView lst >>=
                                       maybe (return ()) (selectLast tv) 
        
        setupEFList :: ListStore FormulaItem -> GuiMonad ()
        setupEFList list = do
            stRef  <- get
            boxTV  <- view (gSatTVFormula . gBoxTreeView)
            addb   <- view (gSatTVFormula . gAddFButton)
            delb   <- view (gSatTVFormula . gDelFButton)
            checkb <- view (gSatTVFormula . gCheckFButton)
            infoSb <- view gSatInfoStatusbar
            content <- ask
            io $
                treeViewNewWithModel list >>= \tv ->
                cleanBoxTV boxTV >>
                containerAdd boxTV tv >>
                
                treeViewGetSelection tv >>= \seltv ->
                treeSelectionSetMode seltv SelectionSingle >>
                treeViewSetHeadersVisible tv False >>
                treeViewSetModel tv list >>


                after tv keyPressEvent (do
                   key <- eventKeyName
                   let actions = [ ("Insert", addItem list content stRef)
                                 , ("Delete", delItem list tv content stRef)
                                 ]
                   maybe (return False) (io >=> const (return False)) (lookup key actions)) >>


                -- Evento para agregar una fórmula
                onToolButtonClicked addb (addItem list content stRef) >>
                                          
                -- Evento para borrar fórmula.
                onToolButtonClicked delb (delItem list tv content stRef)  >>
                
                -- Evento para chequear las fórmulas.
                onToolButtonClicked checkb (evalGState content stRef $ 
                                                makeModelAndCheckFormula list tv)>>

                on tv cursorChanged (updateStatus infoSb list tv) >>                
                
                treeViewColumnNew >>= \colName ->
                
                cellRendererTextNew >>= \renderer ->
                set renderer entryFormAttrs >>

                on renderer editingStarted setEditingStartedEntryIconTable >>

                on renderer edited (\tp s -> treeModelGetIter list tp >>= \(Just ti) ->
                                             updateFormula stRef list s ti >>
                                             updateFList content stRef list >>
                                             evalGState content stRef addToUndo >> 
                                             updateStatus infoSb list tv >>
                                             set renderer editedFormStyle >>
                                             return ()) >>
                
                cellLayoutPackStart colName renderer True >>
                cellLayoutSetAttributes colName renderer list setRowStyle >>
                treeViewAppendColumn tv colName >>
                
                -- Icono de estado de fórmula:
                cellRendererTextNew >>= \fstate ->
                cellLayoutPackStart colName fstate False >>
                cellLayoutSetAttributes colName fstate list (const statusTextStyle) >>                
                treeViewColumnSetSizing colName TreeViewColumnAutosize >>
                                                    
                cellLayoutSetAttributes colName fstate list 
                                    (\ind -> statusTextStyle ++ [ cellText := fStateText $ fiState ind ]) >>
                widgetShowAll boxTV >>                
                return ()

            return ()

        setEditingStartedEntryIconTable :: Widget -> TreePath -> IO ()
        setEditingStartedEntryIconTable w _ = do 
                let entry = castToEntry w
                -- esta primer línea es la que permite que Ctrl+V funcione
                _ <- grabAdd entry
                _ <- on entry unrealize (grabRemove entry)
                _ <- on entry entryPopulatePopup 
                   (\menu ->
                          addSubMenu menu entry "Símbolo" symbolsMenu >>
                          addSubMenu menu entry "Predicado" predicatesMenu >>
                          addSubMenu menu entry "Relación" relationsMenu >>
                   widgetShowAll menu)

                return ()

        selectLast :: TreeView -> TreeIter -> IO ()
        selectLast tv iter = treeViewGetSelection tv >>= \sel ->
                             treeSelectionSelectIter sel iter
        
        makeModelAndCheckFormula :: ListStore FormulaItem -> TreeView -> GuiMonad ()
        makeModelAndCheckFormula list tv = get >>= \stRef -> 
                                           useG (gSatBoard . to visualToModel) >>= \model -> 
                                           view gSatInfoStatusbar >>= \infoSb -> do
            updateStateField gSatModel model
            io $ treeModelForeach list (checkFormula stRef list) >>
                 updateStatus infoSb list tv 

        updateStatus infoSb list tv = treeViewGetSelection tv >>= 
                                       treeSelectionGetSelected >>=
                                       updateStatusBar infoSb list


        updateStatusBar :: Statusbar -> ListStore FormulaItem -> 
                           Maybe TreeIter -> IO ()
        updateStatusBar infoSb _ Nothing = do
                        ctx <- statusbarGetContextId infoSb ("Line" :: Text)
                        _ <- statusbarPop infoSb ctx
                        _ <- statusbarPush infoSb ctx 
                                ("Elija una fórmula para ver su condición" :: Text)
                        return ()
        updateStatusBar infoSb list (Just iter) = do
                        let i = listStoreIterToIndex iter
                        
                        ctx <- statusbarGetContextId infoSb ("Line" :: Text)
                        fi <- listStoreGetValue list i
                        _ <- statusbarPop infoSb ctx
                        _ <- statusbarPush infoSb ctx (show $ fiState fi)
                        return ()

        cleanBoxTV :: ScrolledWindow -> IO ()
        cleanBoxTV sw = containerGetChildren sw >>= mapM_ (containerRemove sw)

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
updateFList content stRef list = listStoreToList list >>= 
                                 evalGState content stRef . 
                                   updateStateField gSatFList . fiListTOfList

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
    listStoreSetValue store ind (fi & fistate .~ state sgn model (fi ^. finame)) >> 
    return False
    where state sgn model = either (ParserError . getErrString) (check model) .
                                     parseSignatureFormula sgn
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
                         editableInsertText entry (unpack s) >> 
                         return ()) >>
                      containerAdd menu item >>
                      return ()
    
    -- Hay que setear eventos para cada item, de manera que se pegue
    -- el símbolo en el entry
    return menu
    
    
relationsMenu :: Entry -> IO Menu
relationsMenu entry = predsRelsMenu entry rels
              where rels = S.toList $ S.map rname (relations figuras)
    
predicatesMenu :: Entry -> IO Menu
predicatesMenu entry = predsRelsMenu entry preds
               where preds = S.toList $ S.map pname (predicates figuras)


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


