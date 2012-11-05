-- | Módulo para la lista de iconViews de predicados.
module Sat.GUI.PredicateList where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.Rendering.Cairo.SVG

import Control.Monad
import Control.Monad.Trans.RWS (ask,get,evalRWST)

import Lens.Family

import qualified Data.Map as M (insert)
import Data.Maybe

import Sat.GUI.GState
import Sat.GUI.IconTable

import Sat.Core
import Sat.Signatures.Figures

type MakeIcon = Predicate -> GuiMonad IconT

-- | La configuración de la lista de figuras propiamente hablando.
configPredicateList :: [([Predicate],MakeIcon)] -> GuiMonad ()
configPredicateList = foldM_ configPredList (return ())

configPredList :: GuiMonad () -> ([Predicate],MakeIcon) -> GuiMonad (GuiMonad ())
configPredList makeSep (ps,makeIcon) = ask >>= \content -> do
        let predBox = content ^. gSatPredBox
        
        t <- io $ tableNew 1 1 False
        
        makeSep
        io $ boxPackStart predBox t PackNatural 0
        
        io $ widgetShowAll t
        
        iconList <- makeIconsT ps
        
        configIconTable 3 t iconList (Just drawPrevFig)
        return $ addSep predBox
    where
        makeIconsT :: [Predicate] -> GuiMonad [IconT]
        makeIconsT = mapM makeIcon
        addSep :: HBox -> GuiMonad ()
        addSep predBox = io $ do
            vs <- vSeparatorNew
            boxPackStart predBox vs PackNatural 2
            widgetShowAll vs
