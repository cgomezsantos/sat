module Sat.GUI.EntryFormula where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.Gdk.Events

import Data.Tree

import Control.Monad.Trans.RWS

import qualified Data.Foldable as F

import Sat.GUI.GState

data FormulaItem = FormulaItem { fiName  :: String }

initialFormulaForest :: GuiMonad (Forest FormulaItem)
initialFormulaForest = io $ do
            entry <- return "Ingresar Fórmula."
            return $ [Node (FormulaItem entry) []]

configEntryFormula :: TreeView -> GuiMonad ()
configEntryFormula tv = initialFormulaForest >>= 
                           io . treeStoreNew >>= 
                           setupEFList
    where
        setupEFList :: TreeStore FormulaItem -> GuiMonad ()
        setupEFList list = do
            content <- ask
            s <- get
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
                -- Acá ghci sugiere usar on obj edited, en lugar de onEdited.
                -- El 1 en treeStoreInsert debería ser el tamaño de la list.
                on renderer edited (\tp s -> treeStoreChange list tp (const $ FormulaItem s) >> 
                                             treeStoreInsert list [] 1 (FormulaItem $ "Ingresar Fórmula.") >>
                                             return ()) >>
                
                cellLayoutPackStart colName renderer False >>
                
                treeViewColumnSetSizing colName TreeViewColumnAutosize >>
                
                cellLayoutSetAttributes colName renderer list 
                                    (\ind -> [ cellText := fiName ind ]) >>
                                    
                treeViewAppendColumn tv colName >>
                return ()
            return ()
