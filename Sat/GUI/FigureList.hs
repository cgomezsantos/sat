-- | Módulo para el iconView de figuras.
module Sat.GUI.FigureList where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.Rendering.Cairo.SVG

import Control.Monad.Trans.RWS (ask,get,evalRWST)

import Lens.Family

import Sat.GUI.SVG
import Sat.GUI.GState
import Sat.GUI.IconTable

import Sat.Core
import Sat.VisualModel
import Sat.Signatures.Figures

iconFigureSize :: (Int,Int)
iconFigureSize = (40,40)

figureList :: [Predicate]
figureList = [triangulo,cuadrado,circulo]

-- | La configuración de la lista de figuras propiamente hablando.
configFigureList :: [Predicate] -> GuiMonad ()
configFigureList list = ask >>= \content -> get >>= \s -> do
        let ftable = content ^. gSatFigTable
        
        iconList <- makeIconsT list
        
        configIconTable 3 ftable iconList
    where
        makeIconsT :: [Predicate] -> GuiMonad [IconT]
        makeIconsT = mapM makeIcon
        makeIcon :: Predicate -> GuiMonad IconT
        makeIcon p = do
            draw  <- drawingIcon [p,grande]
            label <- makeLabelIcon p
            return $ IconT p (Just draw) (Just label)
