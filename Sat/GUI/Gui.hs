{-# Language CPP #-}
module Sat.GUI.Gui where

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)
import Graphics.Rendering.Cairo.SVG

import Control.Monad
import Control.Monad.Trans.RWS

import Control.Lens hiding (set,act)

import Sat.GUI.SVG
import Sat.GUI.File
import Sat.GUI.Board
import Sat.GUI.GState
import Sat.GUI.UndoRedo
import Sat.GUI.SVGBoard
import Sat.GUI.IconTable
import Sat.GUI.FigureList
import Sat.GUI.EntryFormula
import Sat.GUI.FileStatusbar
import Sat.GUI.PredicateList
import Sat.GUI.Settings

import Sat.Signatures.Figures
import Sat.VisualModel(visualToModel)

import Paths_sat

-- | Función principal de la interfaz.
mainSatGui :: Builder -> IO ()
mainSatGui xml = do
    
    (gReader,gState) <- makeGState xml
    
    boardfn <- getDataFileName "board.svg"
    svgboard <- svgNewFromFile boardfn
    
    _ <- runRWST (do configWindow xml
                     configRenderBoard svgboard
                     configEntryFormula []
                     configDrawPieceInBoard 
                     configFigureList figureList
                     configPredicateList [ ([rojo,verde,azul],makeColourIcon)
                                         , ([mediano,grande,chico],makeSizeIcon)
                                         ]
                     configToolBarButtons xml
                     configMenuBarButtons xml
                     configPrevFigDA
                     configFStatusbar)
                 gReader 
                 gState
    return ()

configFStatusbar :: GuiMonad ()
configFStatusbar = updateFileStatusbarNewFile

configPrevFigDA :: GuiMonad ()
configPrevFigDA = ask >>= \content -> get >>= \stref -> do
    let pfda  = content ^. gSatPrevFigDA
    
    _ <- io $ pfda `onExpose` \_ -> do
        st <- readRef stref
        let preds = st ^. (gSatPieceToAdd . eaPreds)
        svgelem <- io $ generateSVG boardMain boardMod [] preds
    
        widgetSetSizeRequest pfda 90 30
        drawWindow <- widgetGetDrawWindow pfda
    
        (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize pfda
        
        drawWindowClear drawWindow
        renderWithDrawable drawWindow (renderPred drawWidth drawHeight svgelem)
        return True
    
    return ()
    
makeColourIcon :: MakeIcon
makeColourIcon p = do
    draw  <- drawingIcon [cuadrado,grande,p]
    label <- makeLabelIcon p
    return $ IconT p (Just draw) (Just label)

makeSizeIcon :: MakeIcon
makeSizeIcon p = do
    draw  <- drawingIcon [cuadrado,p]
    label <- makeLabelIcon p
    return $ IconT p (Just draw) (Just label)
    
-- | Genera el estado inicial de la mónada.
makeGState :: Builder -> IO (GReader,GStateRef) 
makeGState xml = do
        
        window <- builderGetObject xml castToWindow "mainWindow"
        drawingArea <- builderGetObject xml castToDrawingArea "drawingarea"
        prevFigda   <- builderGetObject xml castToDrawingArea "prevFigda"
        figureTable <- builderGetObject xml castToTable "figureTable"
        predBox     <- builderGetObject xml castToHBox "predicateBox"
        bPaned      <- builderGetObject xml castToHPaned "boardPaned"
        iStatusbar  <- builderGetObject xml castToStatusbar "infoStatusbar"
        fStatusbar  <- builderGetObject xml castToStatusbar "fileStatusbar"
        
        scrollBoxTV  <- builderGetObject xml castToScrolledWindow "scrollBoxTV"
        scrollPredicateWindow <- builderGetObject xml castToScrolledWindow "scrollPredicateWindow"
        buttonAddF   <- builderGetObject xml castToToolButton "addFormula"
        buttonDelF   <- builderGetObject xml castToToolButton "deleteFormula"
        buttonCheckF <- builderGetObject xml castToToolButton "checkFormulas"
        
        panedSetPosition bPaned 88

        set scrollPredicateWindow [ scrolledWindowHscrollbarPolicy := PolicyAutomatic
                                  , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                                  ]
        set scrollBoxTV [ scrolledWindowHscrollbarPolicy := PolicyAutomatic
                                  , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                                  ]

        let satTVFormula = SatTVFormulaItem scrollBoxTV buttonAddF buttonDelF buttonCheckF 
            
            pieceToAdd = ElemToAdd [] [] 0
            initboard = boardDefault
            initModel = visualToModel initboard
       
        gState <- newRef $ GState initboard
                                  []
                                  pieceToAdd
                                  initModel
                                  Nothing
                                  Nothing
                                  Nothing
                                  ([URInfo initboard ["Ingresar Fórmula"] pieceToAdd],0)
                                  
        let gReader = GReader window
                              figureTable
                              drawingArea
                              prevFigda
                              iStatusbar
                              fStatusbar
                              predBox
                              satTVFormula

        return (gReader,gState)
        
-- | Configura los botones del menu de archivo.
configMenuBarButtons :: Builder -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st -> io $ do
    window <- builderGetObject xml castToWindow "mainWindow"
    
    newMFButton    <- builderGetObject xml castToMenuItem "newMenuFileButton"
    saveMFButton   <- builderGetObject xml castToMenuItem "saveMenuFileButton"
    saveAsMFButton <- builderGetObject xml castToMenuItem "saveAsMenuFileButton"
    loadMFButton   <- builderGetObject xml castToMenuItem "loadMenuFileButton"
    quitB          <- builderGetObject xml castToMenuItem "quitButton"
    
    undoButton <- builderGetObject xml castToMenuItem "undoitem"
    redoButton <- builderGetObject xml castToMenuItem "redoitem"
    
    _ <- mapM_ setItemAction [ 
            (newMFButton,    eval createNewBoard content st)
          , (saveMFButton,   eval saveBoard content st)
          , (saveAsMFButton, eval saveAsBoard content st)
          , (loadMFButton,   eval loadBoard content st)
          , (quitB,          widgetDestroy window)    
          , (undoButton ,    eval undoAction content st)
          , (redoButton ,    eval redoAction content st)
          ]
    return ()

setItemAction :: MenuItemClass object => (object, IO ()) -> IO (ConnectId object)
#if MIN_VERSION_gtk(0,12,4)
setItemAction (but,act) = on but menuItemActivate act
#else
setItemAction (but,act) = on but menuItemActivated act
#endif

setToolItemAction :: ToolButtonClass object => (object, IO ()) -> IO (ConnectId object)
setToolItemAction (but,act) = onToolButtonClicked but act

    
-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configToolBarButtons :: Builder -> GuiMonad ()
configToolBarButtons xml = ask >>= \content -> get >>= \st ->
    io $ do
    
    newFButton    <- builderGetObject xml castToToolButton "newFileButton"
    saveFButton   <- builderGetObject xml castToToolButton "saveFileButton"
    saveAsFButton <- builderGetObject xml castToToolButton "saveAsFileButton"
    loadFButton   <- builderGetObject xml castToToolButton "loadFileButton"
    
    _ <- mapM_ setToolItemAction [
            (newFButton, eval (createNewBoard >> createNewEntryFormula) content st)
          , (saveFButton, eval saveBoard content st)
          , (saveAsFButton, eval saveAsBoard content st)
          , (loadFButton, eval loadBoard content st)
          ]
    
    return ()

-- | Configura la ventana principal.
configWindow :: Builder -> GuiMonad ()
configWindow xml = io $ do
            window <- builderGetObject xml castToWindow "mainWindow"
            windowMaximize window
            widgetShowAll window
            _ <- onDestroy window mainQuit
            return ()

eval :: GuiMonad () -> GReader -> GStateRef -> IO ()
eval action content str = void $ evalRWST action content str
