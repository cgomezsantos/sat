module Sat.GUI.EntryFormula where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)

import Data.Tree

import Control.Monad.Trans.RWS

import Sat.GUI.GState

data DeclState = DNoState
               | DUnknown
               | DChecked
               | DError

data FormulaItem = FormulaItem { fiName  :: Entry }

initialFormulaForest :: GuiMonad (Forest FormulaItem)
initialFormulaForest = io $ do
            entry <- entryNew
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
--             io $               
--                 treeViewGetColumn tv 0 >>=                
--                 F.mapM_ (treeViewRemoveColumn tv) >>
--                 
--                 treeViewColumnNew >>= \colName ->
--                 treeViewSetHeadersVisible tv False >>
--                 treeViewSetModel tv list >>
--                 
--                 cellRendererTextNew >>= \renderer ->
--                 cellLayoutPackStart colName renderer False >>
--                 
--                 treeViewColumnSetSizing colName TreeViewColumnAutosize >>
--                 cellLayoutSetAttributes colName renderer list 
--                                     (\ind -> [ cellText := ind ^. declName ]) >>
--                                     
--                 treeViewAppendColumn tv colName >>
--                 treeViewGetSelection tv >>= \tree ->
--                 return ()
            return ()