-- | Módulo para el iconView de figuras.
module Sat.GUI.FigureList where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.Rendering.Cairo.SVG

import Control.Monad.Trans.RWS (ask,get,evalRWST)

import Lens.Family

import Sat.GUI.GState

import Sat.Core

iconFigureSize :: (Int,Int)
iconFigureSize = (40,40)

-- | Genera un listStore de figureItem, concentrando la info del svg, pixbuf 
-- y nombre.
listFigure :: [(FilePath,Predicate)] -> IO (ListStore FigureItem)
listFigure fpps = mapM addItem fpps >>= listStoreNew
    where 
        addItem :: (FilePath,Predicate) -> IO FigureItem
        addItem (fp, p) = pixbufNewFromFileAtScale fp w h False >>= \pb-> 
                          svgNewFromFile fp >>= \svg ->
                          return (FigureItem (pname p) svg pb)
        w = fst iconFigureSize
        h = snd iconFigureSize
        
-- | La configuración de la lista de figuras propiamente hablando.
configFigureList :: ListStore FigureItem -> GuiMonad ()
configFigureList list = ask >>= \content -> get >>= \s -> io $ do
        let iv = content ^. gSatFigIV
            scol = makeColumnIdString 1
            pcol = makeColumnIdPixbuf 2
        
        listSize <- listStoreGetSize list
        
        iconViewSetTextColumn iv scol
        iconViewSetPixbufColumn iv pcol
        
        customStoreSetColumn list scol (^. fiName)
        customStoreSetColumn list pcol (^. fiPixbuf)
        
        set iv [ iconViewModel := Just list
               , iconViewPixbufColumn := pcol
               , iconViewTextColumn := scol
               , iconViewRowSpacing := 0
               , iconViewMargin := 0
               , iconViewSelectionMode := SelectionSingle
               ]
        
        eventsFigureList content s iv list
        widgetShowAll iv

eventsFigureList :: GReader -> GStateRef -> 
                    IconView -> ListStore FigureItem -> IO ()
eventsFigureList content s iv list = do
            iv `on` itemActivated $ \path ->
                evalRWST (oneSelection list path) content s >> return ()
            return ()

oneSelection :: ListStore FigureItem -> TreePath -> GuiMonad ()
oneSelection list path = do
                fig <- io $ getElem list path
                updateGState ((<~) (gSatPieceToAdd . paFig) fig)
