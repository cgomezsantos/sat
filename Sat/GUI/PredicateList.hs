-- | Módulo para la lista de iconViews de predicados.
module Sat.GUI.PredicateList where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.Rendering.Cairo.SVG

import Control.Monad.Trans.RWS (ask)

import Lens.Family

import Data.Maybe

import Sat.GUI.GState

import Sat.Core

type EditSVG = SVG -> SVG

-- | Tenemos la información sobre, el nombre del icono, la figura del icono
-- y que acción toma en la edición de un SVG.
data PredicateItem = PredicateItem { piName   :: Maybe String
                                   , piPixbuf :: Maybe Pixbuf
                                   , piTrans  :: EditSVG
                                   }

iconPredicateSize :: (Int,Int)
iconPredicateSize = (40,40)

-- | Crea un listStore partiendo de una terna. La primera componente
-- identifica el nombre del icono, la segunda el lugar de donde
-- cargar la imagen del icono y la tercera como modificar un SVG.
listPredicateFromFile :: [(Maybe String,Maybe FilePath,EditSVG)] -> 
                         IO (ListStore PredicateItem)
listPredicateFromFile fpps = mapM addItem fpps >>= listStoreNew
    where 
        w = fst iconPredicateSize
        h = snd iconPredicateSize
        addItem :: (Maybe String,Maybe FilePath,EditSVG) -> IO PredicateItem
        addItem (ms,Nothing,f) = return (PredicateItem ms Nothing f)
        addItem (ms,Just fp,f) = pixbufNewFromFileAtScale fp w h False >>= 
                                 \pb -> return (PredicateItem ms (Just pb) f)

-- | Crea un listStore apartir de una lista de PredicateItem.
listPredicate :: [PredicateItem] -> IO (ListStore PredicateItem)
listPredicate = listStoreNew

-- | Configura un iconView en base al listStore correspondiente.
setupPredicateList :: IconView -> ListStore PredicateItem -> GuiMonad ()
setupPredicateList iv list = io $ do
        listSize <- listStoreGetSize list
        
        ls <- listStoreToList list
        
        let hasName = catMaybes $ map piName ls
        -- Una condición importante es que asumo, de momento, que
        -- el hasName es vacío o una lista de todos Just.
        case hasName of
            [] -> return ()
            _  -> configName
        
        let hasPixbuf = catMaybes $ map piPixbuf ls
        -- Una condición importante es que asumo, de momento, que
        -- el hasPixbuf es vacío o una lista de todos Just.
        case hasPixbuf of
            [] -> return ()
            _  -> configPixbuf
            
        set iv [ iconViewRowSpacing := 0
               , iconViewMargin := 0
               , iconViewSelectionMode := SelectionSingle
               ]
        
        widgetShowAll iv
    where
        configName :: IO ()
        configName = do
            let scol = makeColumnIdString 1
            customStoreSetColumn list scol (fromJust . piName)
            set iv [ iconViewTextColumn := scol ]
        configPixbuf :: IO ()
        configPixbuf = do
            let pcol = makeColumnIdPixbuf 2
            customStoreSetColumn list pcol (fromJust . piPixbuf)
            set iv [ iconViewPixbufColumn := pcol ]

-- | Configuramos la lista de iconViews en base a una lista de predicados.
configPredicateIVs :: [ListStore PredicateItem] -> GuiMonad ()
configPredicateIVs = mapM_ configPred
    where
        configPred :: ListStore PredicateItem -> GuiMonad ()
        configPred pitem = ask >>= \content -> do
            let predBox = content ^. gSatPredBox
            
            iv <- io $ iconViewNewWithModel pitem
            
            setupPredicateList iv pitem
            
            io $ boxPackStart predBox iv PackGrow 2
            io $ widgetShowAll predBox
    