-- | Módulo para el iconView de figuras.
module Sat.GUI.FigureList where

import Control.Monad.Trans.RWS (ask)

import Control.Lens

import Sat.GUI.SVG
import Sat.GUI.GState
import Sat.GUI.IconTable

import Sat.Signatures.Figures

iconFigureSize :: (Int,Int)
iconFigureSize = (40,40)

figureList :: [Predicate]
figureList = [triangulo,cuadrado,circulo]

-- | La configuración de la lista de figuras propiamente hablando.
configFigureList :: [Predicate] -> GuiMonad ()
configFigureList list = ask >>= \content -> do
        let ftable = content ^. gSatFigTable
        
        iconList <- makeIconsT list
        
        configIconTable 3 ftable iconList (Just drawPrevFig)
    where
        makeIconsT :: [Predicate] -> GuiMonad [IconT]
        makeIconsT = mapM makeIcon
        makeIcon :: Predicate -> GuiMonad IconT
        makeIcon p = do
            draw  <- drawingIcon [p,grande]
            label <- makeLabelIcon p
            return $ IconT p (Just draw) (Just label)
