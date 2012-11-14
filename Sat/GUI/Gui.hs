-- | Módulo principal de la interfaz.
module Sat.GUI.Gui where

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)
-- import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Control.Monad
import Control.Monad.Trans.RWS
import Control.Applicative

import Lens.Family

import Data.Maybe
import Data.Reference (newRef,readRef)
import qualified Data.Map as M (empty)

import Sat.GUI.SVG
import Sat.GUI.SVGBoard
import Sat.GUI.File
import Sat.GUI.Board
import Sat.GUI.GState
import Sat.GUI.IconTable
import Sat.GUI.SymbolList
import Sat.GUI.FigureList
import Sat.GUI.EntryFormula
import Sat.GUI.PredicateList

import Sat.VisualModels.FiguresBoard
import Sat.Signatures.Figures
import Sat.VisualModel(visualToModel)

import qualified Sat.Example.Example as Example(b)


-- | Función principal de la interfaz.
main :: IO ()
main = do
    initGUI

    xml <- builderNew
    builderAddFromFile xml "Sat/GUI/sat.ui"

    (gReader,gState) <- makeGState xml
        
    svgboard <- svgNewFromFile "Sat/GUI/board.svg"
    
    formulaTV <- builderGetObject xml castToTreeView "formulaTV"
    buttonAddF <- builderGetObject xml castToToolButton "addFormula"
    buttonDelF <- builderGetObject xml castToToolButton "deleteFormula"
    buttonCheckF <- builderGetObject xml castToToolButton "checkFormulas"
    
    runRWST (do configWindow xml
                configRenderBoard svgboard
                configEntryFormula [] formulaTV buttonAddF buttonDelF buttonCheckF
                configDrawPieceInBoard svgboard
                configFigureList figureList
                configPredicateList [ ([rojo,verde,azul],makeColourIcon)
                                    , ([mediano,grande,chico],makeSizeIcon)
                                    ]
                configToolBarButtons xml
                configMenuBarButtons xml
                configPrevFigDA
                configSymbolList
            ) gReader gState
    
    mainGUI

configPrevFigDA :: GuiMonad ()
configPrevFigDA = ask >>= \content -> get >>= \stref -> do
    let pfda  = content ^. gSatPrevFigDA
    
    io $ pfda `onExpose` \_ -> do
        st <- readRef stref
        let preds = st ^. (gSatPieceToAdd . eaPreds)
        svgelem <- io $ generateSVG boardMain boardMod preds
    
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
    label <- makeLabelIcon p
    return $ IconT p Nothing (Just label)
    
-- | Genera el estado inicial de la mónada.
makeGState :: Builder -> IO (GReader,GStateRef) 
makeGState xml = do
        
        window <- builderGetObject xml castToWindow "mainWindow"
        drawingArea <- builderGetObject xml castToDrawingArea "drawingarea"
        prevFigda   <- builderGetObject xml castToDrawingArea "prevFigda"
        figureTable <- builderGetObject xml castToTable "figureTable"
        predBox     <- builderGetObject xml castToHBox "predicateBox"
        bPaned      <- builderGetObject xml castToHPaned "boardPaned"
        mStatusbar  <- builderGetObject xml castToStatusbar "mainStatusbar"
        
        mModelButton <- builderGetObject xml castToToolButton "makeModelButton"
        
        symFrameB  <- builderGetObject xml castToToggleToolButton "symFrameButton"
        symFrame   <- builderGetObject xml castToFrame "symFrame"
        goLeftBox  <- builderGetObject xml castToHBox "symGoLeftBox"
        scrollW    <- builderGetObject xml castToScrolledWindow "swSymbolList"
        symIV      <- builderGetObject xml castToIconView "symbolList"
        goRightBox <- builderGetObject xml castToHBox "symGoRightBox"
        
        panedSetPosition bPaned 88
        
        let satSymListST = SatSymList symFrame goLeftBox scrollW symIV goRightBox
            satToolbarST = SatToolbar mModelButton symFrameB
            
            pieceToAdd = ElemToAdd [] [] 0
            initboard = boardDefault
            initModel = visualToModel initboard
       
        gState <- newRef $ GState initboard
                                  pieceToAdd
                                  initModel
                                  Nothing
                                  
        let gReader = GReader window
                              figureTable
                              drawingArea
                              prevFigda
                              mStatusbar
                              predBox
                              satSymListST
                              satToolbarST
        
        return (gReader,gState)
        
-- | Configura los botones del menude archivo.
configMenuBarButtons :: Builder -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st -> io $ do
    window <- builderGetObject xml castToWindow "mainWindow"
    
    newMFButton    <- builderGetObject xml castToMenuItem "newMenuFileButton"
    saveMFButton   <- builderGetObject xml castToMenuItem "saveMenuFileButton"
    saveAsMFButton <- builderGetObject xml castToMenuItem "saveAsMenuFileButton"
    loadMFButton   <- builderGetObject xml castToMenuItem "loadMenuFileButton"
    quitB          <- builderGetObject xml castToMenuItem "quitButton"
    
    onActivateLeaf newMFButton    (eval createNewBoard content st)
    onActivateLeaf saveMFButton   (eval saveBoard content st)
    onActivateLeaf saveAsMFButton (eval saveAsBoard content st)
    onActivateLeaf loadMFButton   (eval loadBoard content st)
    onActivateLeaf quitB  $ widgetDestroy window
    return ()
        
-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configToolBarButtons :: Builder -> GuiMonad ()
configToolBarButtons xml = ask >>= \content -> get >>= \st ->
    io $ do
    
    newFButton    <- builderGetObject xml castToToolButton "newFileButton"
    saveFButton   <- builderGetObject xml castToToolButton "saveFileButton"
    saveAsFButton <- builderGetObject xml castToToolButton "saveAsFileButton"
    loadFButton   <- builderGetObject xml castToToolButton "loadFileButton"
    symFButton    <- builderGetObject xml castToToggleToolButton "symFrameButton"
    mModelButton  <- builderGetObject xml castToToolButton "makeModelButton"
    
    onToolButtonClicked newFButton    (eval createNewBoard content st)
    onToolButtonClicked saveFButton   (eval saveBoard content st)
    onToolButtonClicked saveAsFButton (eval saveAsBoard content st)
    onToolButtonClicked loadFButton   (eval loadBoard content st)
    onToolButtonClicked symFButton    (eval configSymFrameButton content st)
    onToolButtonClicked mModelButton  (eval makeModelFromBoard content st)
    
    return ()
        
-- | Configura la ventana principal.
configWindow :: Builder -> GuiMonad ()
configWindow xml = io $ do
            window <- builderGetObject xml castToWindow "mainWindow"
            windowMaximize window
            widgetShowAll window
            onDestroy window mainQuit
            return ()

eval :: GuiMonad () -> GReader -> GStateRef -> IO ()
eval action content str = void $ evalRWST action content str
