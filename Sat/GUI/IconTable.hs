-- | Este módulo representa una grilla con botones, mutuamente exclusivos, 
-- activables.
module Sat.GUI.IconTable where

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get,tableGetSize)
import qualified Graphics.UI.Gtk as G
import Graphics.Rendering.Cairo (save,scale,restore,Render)
import Graphics.Rendering.Cairo.SVG (SVG,svgRender)

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.RWS (ask,get,evalRWST)

import Data.Maybe
import qualified Data.List as L

import Control.Lens

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

tableGetSize :: TableClass o => o -> IO (Int, Int)
tableGetSize table = G.get table tableNRows >>= \x ->
                     G.get table tableNColumns >>= \y ->
                     return (x,y)

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
                let da = itDraw it
                    l  = itLabel it
                
                w <- makeIconWidget da l
                
                tb  <- makeToggleButton w
                configEventToggleButton tb table its mActive
                
                (nr,nc) <- io $ tableGetSize table 
                let (cr',cc') = if nc <= maxNthColums then (nr-1,nc) else (nr,1)
                io $ do tableAttach table tb cc' (cc'+1) cr' (cr'+1) [] [] 0 0
                        widgetShowAll tb
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
        _ <- on tb buttonReleaseEvent (actionTb' content st)
        _ <- tb `onToggled` actionTb content st
        return ()
    where
        actionTb' :: GReader -> GStateRef -> EventM EButton Bool
        actionTb' content stRef = do
                    button <- G.eventButton
                    case button of
                         RightButton -> io $ takePredName content stRef >> 
                                             return True
                         _ -> return False
                         
        takePredName :: GReader -> GStateRef -> IO ()
        takePredName content stRef = do
            st <- readRef stRef
            let meitInfo = st ^. gSatEntryIconTable
            
            case meitInfo of
                Nothing -> return ()
                Just eitInfo -> do
                    let boxTV = content ^. (gSatTVFormula . gBoxTreeView)
                    
                    [tvw] <- containerGetChildren boxTV
                    [tbBox] <- io $ containerGetChildren tb
                    ctb     <- io $ containerGetChildren $ castToBox tbBox
                    
                    let tp = eitInfo ^. eitTp
                        tv = castToTreeView tvw
                        Just it = L.find (check ctb) its
                    
                    pos <- editableGetPosition (eitInfo ^. eitEntry)
                    
                    let string = eitInfo ^. eitText
                        str = take pos string ++ pname (itPred it)
                        ing = drop pos string
                        
                        neweitInfo = Just $ (.~) (eitText) (str ++ ing) eitInfo
                    
                    writeRef stRef ((.~) (gSatEntryIconTable) neweitInfo st)
                    
                    [col] <- treeViewGetColumns tv
                    treeViewSetCursor tv tp (Just (col,True))
                    
                    return ()
        
        -- Acción a realizar cuando el botón se activa.
        actionTb :: GReader -> GStateRef -> IO ()
        actionTb content st = do
            tbs      <- containerGetChildren t
            isActive <- toggleButtonGetActive tb
            let action = fromMaybe (return ()) mActive
                        
            _ <- flip (`evalRWST` content) st $
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
        updateElemToAdd fchange tb' = do
            st <- getGState
            
            [tbBox] <- io $ containerGetChildren (castToToggleButton tb')
            ctb     <- io $ containerGetChildren $ castToBox tbBox
            
            let etaPreds = st ^. (gSatPieceToAdd . eaPreds)
                Just it = L.find (check ctb) its
                etaPreds' = fchange (itPred it) etaPreds
            
            updateStateField (gSatPieceToAdd . eaPreds) etaPreds'
        
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
    svgelem <- generateSVG boardMain boardMod [] ps 
    
    _ <- da `on` realize $ do
        _ <- da `onExpose` \_ -> do
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
    
    svgelem <- generateSVG boardMain boardMod [] preds 
    
    drawWindow <- widgetGetDrawWindow pfda
            
    (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize pfda
    
    drawWindowClear drawWindow
    renderWithDrawable drawWindow (renderPred drawWidth drawHeight svgelem)
    
    return ()

renderPred :: Double -> Double -> SVG -> Render ()
renderPred w h svgelem = do save
                            scale (w/200) (h/200)
                            _ <- svgRender svgelem
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
