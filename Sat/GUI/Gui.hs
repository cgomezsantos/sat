-- | Módulo principal de la interfaz.
module Sat.GUI.Gui where

import Graphics.UI.Gtk hiding (eventButton,eventRegion,eventClick,get)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo.SVG

import Control.Monad.Trans.RWS
import Control.Applicative

import Data.Maybe
import Data.Reference (newRef)

import Sat.GUI.Board
import Sat.GUI.GState
import Sat.GUI.SymbolList
import Sat.GUI.FigureList
import Sat.GUI.PredicateList

import Sat.VisualModels.FiguresBoard
import Sat.Signatures.Figures

-- | Función principal de la interfaz.
main :: IO ()
main = do
    initGUI
    
    xml <- fromMaybe (error msgErrGladeNotFound) <$> xmlNew "sat.glade"
    
    (gReader,gState) <- makeGState xml
    
    -- /////////////// Esto esta hardCodeado para testeo ///////////////////
    -- La idea es que esto va en algún archivo de configuración.
    lf <- listFigure [("Sat/GUI/SVG/cuadrado.svg",cuadrado)
                     ,("Sat/GUI/SVG/triangulo.svg",triangulo)
                     ,("Sat/GUI/SVG/circulo.svg",circulo)
                     ]
    
    cl <- listPredicateFromFile [ (Nothing,(Just "Sat/GUI/SVG/testRed.svg"),id)
                                , (Nothing,(Just "Sat/GUI/SVG/testGreen.svg"),id)
                                , (Nothing,(Just "Sat/GUI/SVG/testBlue.svg"),id)
                                ]
                        
    tl <- listPredicate [ PredicateItem (Just "Grande") Nothing id
                        , PredicateItem (Just "Normal") Nothing id
                        , PredicateItem (Just "Chico")  Nothing id
                        ]
    -- ////////////////////////////////////////////////////////////////////
    
    board <- svgNewFromFile "Sat/GUI/SVG/board.svg"
    
    runRWST (do configWindow xml
                configMenuBarButtons xml
                configFigureList lf
                renderBoard board
                configPredicateIVs [cl,tl]
                configSymbolList
            ) gReader gState
    
    mainGUI

-- | Genera el estado inicial de la mónada.
makeGState :: GladeXML -> IO (GReader,GStateRef) 
makeGState xml = do
        
        drawingArea <- xmlGetWidget xml castToDrawingArea "drawingarea"
        figureIV    <- xmlGetWidget xml castToIconView "figureIV"
        predBox     <- xmlGetWidget xml castToHBox "predicatesBox"
        bPaned      <- xmlGetWidget xml castToHPaned "boardPaned"
        
        symFrameB <- xmlGetWidget xml castToToggleToolButton "symFrameButton"
        
        symFrame   <- xmlGetWidget xml castToFrame "symFrame"
        goLeftBox  <- xmlGetWidget xml castToHBox "symGoLeftBox"
        scrollW    <- xmlGetWidget xml castToScrolledWindow "swSymbolList"
        symIV      <- xmlGetWidget xml castToIconView "symbolList"
        goRightBox <- xmlGetWidget xml castToHBox "symGoRightBox"
        
        panedSetPosition bPaned 150
        
        let satSymListST = SatSymList symFrame goLeftBox scrollW symIV goRightBox
            satToolbarST = SatToolbar symFrameB 
       
        gState <- newRef $ GState boardDefault
        let gReader = GReader figureIV 
                              drawingArea 
                              predBox 
                              satSymListST
                              satToolbarST
        
        return (gReader,gState)

-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configMenuBarButtons :: GladeXML -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st ->
        io $ do
        
        symFButton    <- xmlGetWidget xml castToToggleToolButton "symFrameButton"
        
        onToolButtonClicked symFButton (eval configSymFrameButton content st)
        
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
eval action content str = evalRWST action content str >> return ()

-- | Mensaje de error en caso de no encontrar el archivo glade correspondiente.
msgErrGladeNotFound :: String
msgErrGladeNotFound = "Archivo fun.glade no encontrado"
