module Sat.GUI.IconTable where

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Control.Monad
import Control.Monad.Trans.RWS (ask,get,evalRWST)

import Data.Maybe
import qualified Data.List as L

import Lens.Family

import Sat.GUI.SVG
import Sat.GUI.GState

import Sat.Core

data IconT = IconT { itPred  :: Predicate 
                   , itDraw  :: Maybe DrawingArea
                   , itLabel :: Maybe Label
                   }
                   
instance Show IconT where
    show = show . itPred
                   
configIconTable :: Int -> Table -> [IconT] -> GuiMonad ()
configIconTable cc t its = forM_ its (addIconToTable cc t)
    where
        addIconToTable :: Int -> Table -> IconT -> GuiMonad ()
        addIconToTable maxNthColums table it = do
                content <- ask
                let da = itDraw it
                    l  = itLabel it
                
                w <- makeIconWidget da l
                
                tb  <- makeToggleButton w
                configEventToggleButton tb table its
                
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
                    
makeToggleButton :: Widget -> GuiMonad ToggleButton
makeToggleButton w = io $ do
            tb <- toggleButtonNew
            buttonSetRelief tb ReliefNone
            containerAdd tb w
            return tb
            
configEventToggleButton :: ToggleButton -> Table -> [IconT] -> GuiMonad ()
configEventToggleButton tb t its = ask >>= \content -> get >>= \st -> io $ do
        tb `onToggled` actionTb content st
        return ()
    where
        actionTb :: GReader -> GStateRef -> IO ()
        actionTb content st = do
            tbs <- containerGetChildren t
            isA <- toggleButtonGetActive tb
            if isA
               then evalRWST updateActiveElemToAdd content st >> 
                    mapM_ deActivateTb tbs
               else void $ evalRWST updateDeActiveElemToAdd content st
--             evalRWST (do 
--                         st <- getGState
--                         let etaPreds = st ^. (gSatPieceToAdd . eaPreds)
--                         io $ putStrLn $ show etaPreds) content st
            return ()

        updateElemToAdd :: (Predicate -> [Predicate] -> [Predicate]) -> 
                           GuiMonad ()
        updateElemToAdd fchange = do
            st <- getGState
            
            [tbBox] <- io $ containerGetChildren tb
            ctb     <- io $ containerGetChildren $ castToBox tbBox
            
            let etaPreds = st ^. (gSatPieceToAdd . eaPreds)
                Just it = L.find (check ctb) its
                etaPreds' = fchange (itPred it) etaPreds
            
            updateGState ((<~) (gSatPieceToAdd . eaPreds) etaPreds')
            
        updateActiveElemToAdd :: GuiMonad ()
        updateActiveElemToAdd = updateElemToAdd (:)
            
        updateDeActiveElemToAdd :: GuiMonad ()
        updateDeActiveElemToAdd = updateElemToAdd L.delete
            
        check :: [Widget] -> IconT -> Bool
        check ctb it = let draw  = fmap castToWidget (itDraw  it)
                           label = fmap castToWidget (itLabel it)
                       in
                        draw == Just (head ctb) || label == Just (head ctb)
            
        deActivateTb :: Widget -> IO ()
        deActivateTb w = 
            let tb' = castToToggleButton w in
            unless (tb' == tb) $ toggleButtonSetActive tb' False

makeLabelIcon :: Predicate -> GuiMonad Label
makeLabelIcon p = io $ labelNew $ Just $ pname p

drawingIcon :: [Predicate] -> GuiMonad DrawingArea
drawingIcon ps = io $ do
        da <- drawingAreaNew
        
        widgetSetSizeRequest da 40 40
        svgelem <- generateSVG ps
        
        da `on` realize $ do
            da `onExpose` \Expose { eventRegion = exposeRegion } -> 
                do
                drawWindow <- widgetGetDrawWindow da
                
                (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da
                
                renderWithDrawable drawWindow $ do
                    save
                    scale (drawWidth/200) (drawHeight/200)
                    svgRender svgelem
                    restore
                return False
            return ()
        return da
