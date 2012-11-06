-- | Módulo principal de la interfaz.
module Sat.GUI.Gui where

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)
import Graphics.UI.Gtk.Glade
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
    
    xml <- fromMaybe (error msgErrGladeNotFound) <$> xmlNew "Sat/GUI/sat.glade"
    
    (gReader,gState) <- makeGState xml
        
    board <- svgNewFromFile "Sat/GUI/board.svg"
    
    formulaTV <- xmlGetWidget xml castToTreeView "formulaTV"
    buttonAddF <- xmlGetWidget xml castToToolButton "addFormula"
    buttonCheckF <- xmlGetWidget xml castToToolButton "checkFormulas"
    
    runRWST (do configWindow xml
                configToolBarButtons xml
                configMenuBarButtons xml
                renderBoard board
                configEntryFormula [] formulaTV buttonAddF buttonCheckF
                configDrawPieceInBoard board
                configFigureList figureList
                configPredicateList [ ([rojo,verde,azul],makeColourIcon)
                                    , ([mediano,grande,chico],makeSizeIcon)
                                    ]
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
        svgelem <- io $ generateSVG preds
    
        widgetSetSizeRequest pfda 90 30
        drawWindow <- widgetGetDrawWindow pfda
    
        (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize pfda
        
        drawWindowClear drawWindow
        renderWithDrawable drawWindow (renderPred drawWidth drawHeight svgelem)
        return True
    
    return ()
    
makeColourIcon :: MakeIcon
makeColourIcon p = do
    draw  <- drawingIcon [cuadrado,p]
    label <- makeLabelIcon p
    return $ IconT p (Just draw) (Just label)

makeSizeIcon :: MakeIcon
makeSizeIcon p = do
    label <- makeLabelIcon p
    return $ IconT p Nothing (Just label)
    
-- | Genera el estado inicial de la mónada.
makeGState :: GladeXML -> IO (GReader,GStateRef) 
makeGState xml = do
        
        drawingArea <- xmlGetWidget xml castToDrawingArea "drawingarea"
        prevFigda   <- xmlGetWidget xml castToDrawingArea "prevFigda"
        figureTable <- xmlGetWidget xml castToTable "figureTable"
        predBox     <- xmlGetWidget xml castToHBox "predicateBox"
        bPaned      <- xmlGetWidget xml castToHPaned "boardPaned"
        iEditBoard <- xmlGetWidget xml castToImage "iconEditBoard"
        
        symFrameB  <- xmlGetWidget xml castToToggleToolButton "symFrameButton"
        symFrame   <- xmlGetWidget xml castToFrame "symFrame"
        goLeftBox  <- xmlGetWidget xml castToHBox "symGoLeftBox"
        scrollW    <- xmlGetWidget xml castToScrolledWindow "swSymbolList"
        symIV      <- xmlGetWidget xml castToIconView "symbolList"
        goRightBox <- xmlGetWidget xml castToHBox "symGoRightBox"
        
        panedSetPosition bPaned 110
        
        let satSymListST = SatSymList symFrame goLeftBox scrollW symIV goRightBox
            satToolbarST = SatToolbar symFrameB 
            
            pieceToAdd = ElemToAdd [] [] 0
            initboard = Example.b
            initModel = visualToModel initboard
       
        gState <- newRef $ GState initboard
                                  pieceToAdd
                                  initModel
                                  
        let gReader = GReader figureTable
                              drawingArea
                              prevFigda
                              iEditBoard
                              predBox
                              satSymListST
                              satToolbarST
        
        return (gReader,gState)
        
-- | Configura los botones del menude archivo.
configMenuBarButtons :: GladeXML -> GuiMonad ()
configMenuBarButtons xml = io $ do
            window <- xmlGetWidget xml castToWindow "mainWindow"
            quitB  <- xmlGetWidget xml castToMenuItem "quitButton"
            
            onActivateLeaf quitB  $ widgetDestroy window
            return ()
        
-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configToolBarButtons :: GladeXML -> GuiMonad ()
configToolBarButtons xml = ask >>= \content -> get >>= \st ->
        io $ do
        
        symFButton   <- xmlGetWidget xml castToToggleToolButton "symFrameButton"
        mModelButton <-xmlGetWidget xml castToToolButton "makeModelButton"
        
        onToolButtonClicked symFButton (eval configSymFrameButton content st)
        onToolButtonClicked mModelButton (eval makeModelFromBoard content st)
        
        return ()
        
-- | Configura la ventana principal.
configWindow :: GladeXML -> GuiMonad ()
configWindow xml = io $ do
            window <- xmlGetWidget xml castToWindow "mainWindow"
            windowMaximize window
            widgetShowAll window
            onDestroy window mainQuit
            return ()

eval :: GuiMonad () -> GReader -> GStateRef -> IO ()
eval action content str = void $ evalRWST action content str

-- | Mensaje de error en caso de no encontrar el archivo glade correspondiente.
msgErrGladeNotFound :: String
msgErrGladeNotFound = "Archivo fun.glade no encontrado"
