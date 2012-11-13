-- | Este módulo representa una grilla con botones, mutuamente exclusivos, 
-- activables.
module Sat.GUI.IconTable where

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.RWS (ask,get,evalRWST)

import Data.Maybe
import qualified Data.List as L

import Lens.Family

import Sat.GUI.SVG
import Sat.GUI.SVGBoard
import Sat.GUI.GState

import Sat.Core

-- | Representa la información de un botón activable de la tabla.
-- Su predicado al estar activado, el dibujo de tener uno y la leyenda
-- del botón, de nuevo, si es que tiene.
data IconT = IconT { itPred  :: Predicate 
                   , itDraw  :: Maybe DrawingArea
                   , itLabel :: Maybe Label
                   }

instance Show IconT where
    show = show . itPred

-- | Configura una tabla con botones, el entero inicial determina la cantidad
-- de columnas máxima de la tabla, luego viene la tabla en si y la lista de
-- iconos a agregar.
-- La tabla que generamos tiene las siguientes restricciones.
-- 1. La lista de iconos para armar la tabla no puede ser vacía.
-- 2. Siempre hay un botón activable activado en todo momento.
configIconTable :: Int -> Table -> [IconT] -> Maybe (GuiMonad ()) -> GuiMonad ()
configIconTable _ _ [] _ = error "Lista de iconos vacía."
configIconTable cc t its mActive = 
            forM_ its (addIconToTable cc t) >>
            io (containerGetChildren t) >>= \tbs ->
            io (toggleButtonSetActive (castToToggleButton $ last tbs) True)
    where
        addIconToTable :: Int -> Table -> IconT -> GuiMonad ()
        addIconToTable maxNthColums table it = do
                content <- ask
                let da = itDraw it
                    l  = itLabel it
                
                w <- makeIconWidget da l
                
                tb  <- makeToggleButton w
                configEventToggleButton tb table its mActive
                
                (cr,cc) <- io $ tableGetSize table 
                (cr',cc') <- return $ if cc <= maxNthColums then (cr-1,cc) else (cr,1)
                
                io $ tableAttach table tb cc' (cc'+1) cr' (cr'+1) [] [] 0 0
                io $ widgetShowAll tb
            where
                makeIconWidget :: Maybe DrawingArea -> Maybe Label -> GuiMonad Widget
                makeIconWidget mda ml = io $ do
                    box <- vBoxNew False 0
                    maybe (return ()) (\da -> boxPackStart box da PackNatural 0) mda
                    maybe (return ()) (\l -> boxPackStart box l PackNatural 0) ml
                    return $ castToWidget  box

-- | Configura el evento de un botón activable.
configEventToggleButton :: ToggleButton -> Table -> [IconT] -> 
                           Maybe (GuiMonad ()) -> GuiMonad ()
configEventToggleButton tb t its mActive = 
        ask >>= \content -> get >>= \st -> io $ do
        tb `onToggled` actionTb content st
        return ()
    where
        -- Acción a realizar cuando el botón se activa.
        actionTb :: GReader -> GStateRef -> IO ()
        actionTb content st = do
            tbs      <- containerGetChildren t
            isActive <- toggleButtonGetActive tb
            let action = fromMaybe (return ()) mActive
            
            flip (`evalRWST` content) st $
                if isActive
                    -- Si el botón se activo.
                    then mapM_ (deActivateTb <&&&> updateDeActiveElemToAdd) tbs >> 
                         updateActiveElemToAdd (castToWidget tb) >> 
                         action
                    -- Si el botón se des-activo.
                    else checkOneActive tbs
            return ()
        
        (<&&&>) :: Monad m => (a -> m ()) -> (a -> m ()) -> a -> m ()
        (<&&&>) f g a = case (&&&) f g a of 
                        (ma, mb) -> ma >> mb
        
        checkOneActive :: [Widget] -> GuiMonad ()
        checkOneActive tbs = do
            tbsA <- mapM (io . toggleButtonGetActive . castToToggleButton) tbs
            unless (or tbsA) $
                io $ toggleButtonSetActive (castToToggleButton $ last tbs) True
        
        updateElemToAdd :: (Predicate -> [Predicate] -> [Predicate]) -> Widget ->
                           GuiMonad ()
        updateElemToAdd fchange tb = do
            st <- getGState
            
            [tbBox] <- io $ containerGetChildren (castToToggleButton tb)
            ctb     <- io $ containerGetChildren $ castToBox tbBox
            
            let etaPreds = st ^. (gSatPieceToAdd . eaPreds)
                Just it = L.find (check ctb) its
                etaPreds' = fchange (itPred it) etaPreds
            
            updateGState ((<~) (gSatPieceToAdd . eaPreds) etaPreds')
        
        updateActiveElemToAdd :: Widget -> GuiMonad ()
        updateActiveElemToAdd = updateElemToAdd addPred
            where addPred p ps = if (pmain p) then p : ps
                                 else ps ++ [p]
        
        updateDeActiveElemToAdd :: Widget -> GuiMonad ()
        updateDeActiveElemToAdd = updateElemToAdd L.delete
        
        check :: [Widget] -> IconT -> Bool
        check ctb it = let draw  = fmap castToWidget (itDraw  it)
                           label = fmap castToWidget (itLabel it)
                       in
                        draw == Just (head ctb) || label == Just (head ctb)
        
        deActivateTb :: Widget -> GuiMonad ()
        deActivateTb w = io $
            let tb' = castToToggleButton w in
            unless (tb' == tb) $ toggleButtonSetActive tb' False

drawingIcon :: [Predicate] -> GuiMonad DrawingArea
drawingIcon ps = do
        da <- io drawingAreaNew
        drawingIMG ps da 40 40
            
-- | Genera el dibujo de un botón.
drawingIMG :: [Predicate] -> DrawingArea -> Int -> Int -> GuiMonad DrawingArea
drawingIMG ps da w h = io $ do
    
    widgetSetSizeRequest da w h
    svgelem <- generateSVG boardMain boardMod ps
    
    da `on` realize $ do
        da `onExpose` \_ -> 
            do
            drawWindow <- widgetGetDrawWindow da
            
            (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da
            
            renderWithDrawable drawWindow (renderPred drawWidth drawHeight svgelem)
            
            return True
        return ()
    return da

-- | Genera una vista previa en base al elemento que se esta por agregar.
drawPrevFig :: GuiMonad ()
drawPrevFig = ask >>= \content -> getGState >>= \st -> io $ do
    let pfda  = content ^. gSatPrevFigDA
        preds = st ^. (gSatPieceToAdd . eaPreds)
    
    svgelem <- generateSVG boardMain boardMod preds
    
    drawWindow <- widgetGetDrawWindow pfda
            
    (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize pfda
    
    drawWindowClear drawWindow
    renderWithDrawable drawWindow (renderPred drawWidth drawHeight svgelem)
    
    return ()

renderPred :: Double -> Double -> SVG -> Render ()
renderPred w h svgelem = do
    save
    scale (w/200) (h/200)
    svgRender svgelem
    restore

-- | Genera un botón activable.
makeToggleButton :: Widget -> GuiMonad ToggleButton
makeToggleButton w = io $ do
            tb <- toggleButtonNew
            buttonSetRelief tb ReliefNone
            containerAdd tb w
            return tb

-- | Genera la leyenda de un botón.
makeLabelIcon :: Predicate -> GuiMonad Label
makeLabelIcon = io . labelNew . Just . pname
