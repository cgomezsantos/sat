-- | Renderiza el board para la interfaz en base a un archivo SVG.
module Sat.GUI.Board where

import Control.Monad
import Control.Monad.Trans.RWS (ask)

import Lens.Family

import Data.Maybe

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick)
import Graphics.UI.Gtk.Gdk.Events

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Sat.GUI.GState

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x,f y)

hSpacing :: Double
hSpacing = 20

-- | Función principal para el render del board.
renderBoard :: SVG -> GuiMonad ()
renderBoard board = ask >>= \content -> io $ do
    let da = content ^. gSatDrawArea
    da `onExpose` \Expose { eventRegion = exposeRegion } -> do
        drawWindow              <- widgetGetDrawWindow da
        (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da

        renderWithDrawable drawWindow $ do
            let (boardWidth, boardHeight) = mapPair fromIntegral $ svgGetSize board
                sideSize = min drawWidth drawHeight - hSpacing
                xoffset  = (drawWidth - sideSize) / 2
                yoffset  = (drawHeight - sideSize) / 2
            region exposeRegion
            
            translate xoffset yoffset
            scale (sideSize / boardWidth) (sideSize / boardHeight)
            
            svgRender board
            
            return False
    return ()
