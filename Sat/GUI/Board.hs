{-# Language DoAndIfThenElse,OverloadedStrings #-}
-- | Renderiza el board para la interfaz en base a un archivo SVG.
module Sat.GUI.Board where

import Control.Lens hiding (set,(<.>))
import Control.Monad 
import Control.Monad.Trans.RWS (ask,evalRWST,get)

import Data.Char (isUpper)
import qualified Data.List as L
import Data.Text (Text)

import Graphics.UI.Gtk hiding (get)
import Graphics.Rendering.Cairo hiding (x,y,width,height,Region)
import Graphics.Rendering.Cairo.SVG (SVG,svgGetSize,svgRender,svgGetSize,svgRender)



import Sat.Core
import Sat.VisualModels.FiguresBoard

import Sat.GUI.SVG hiding ((<>))
import Sat.GUI.GState
import Sat.GUI.Settings
import Sat.GUI.SVGBoard
import Sat.GUI.IconTable
import Sat.GUI.FileStatusbar

data ErrConstantCheck = InvalidLong 
                      | InvalidCase 
                      | InvalidDup
                      
data ConstantCheck = ConstantErr ErrConstantCheck
                   | ConstantOk [String]


(<>) :: ConstantCheck -> ConstantCheck -> ConstantCheck
ConstantErr e <> _ = ConstantErr e
ConstantOk cs <> ck = cs <.> ck
  where (<:>) :: String -> ConstantCheck -> ConstantCheck
        c <:> (ConstantOk ks') | c `elem` ks' = ConstantErr InvalidDup
                               | otherwise   = ConstantOk (c:ks')
        _ <:> ConstantErr e = ConstantErr e

        (<.>) :: [String] -> ConstantCheck -> ConstantCheck
        [] <.> ConstantOk ks' = ConstantOk ks'
        (k:ks) <.> ConstantOk ks' = k <:> (ks <.> ConstantOk ks')
        _ <.> ConstantErr err = ConstantErr err


instance Show ErrConstantCheck where
    show InvalidLong = "Los nombres de constantes deben tener a lo sumo dos letras"
    show InvalidCase = "Los nombres de constantes deben ser en mayúscula"
    show InvalidDup  = "Constante interpretada por otro elemento"


hSpacing :: Double
hSpacing = 20

configDrag :: DrawingArea -> GReader -> GStateRef -> IO ()
configDrag da cnt stRef = do
        dragSourceSet da [Button1] [ActionCopy]
        dragSourceSetIconStock da stockAdd

        dragSourceAddTextTargets da

        dragDestSet da [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
        dragDestAddTextTargets da
        
        winpop <- configDNDIcon stRef
        
        _ <- da `on` dragDataGet $ \_ _ _ -> selectionDataSetText ("dnd" :: Text) >> return ()
        
        _ <- da `on` dragBegin $ \dc -> do
                                    tempDelete
                                    widgetQueueDraw da
                                    dragSetIconWidget dc winpop 30 30
                                    widgetShowAll winpop
        return ()
    where
        tempDelete :: IO ()
        tempDelete = evalGState cnt stRef $ do
            st <- getGState
            let squareSrc = st ^. gSatDNDSrcCoord
            whenM squareSrc (return ()) 
                            (\el -> deleteElemBoardAt (el ^. _1))
            return ()

configDNDIcon :: GStateRef -> IO Widget
configDNDIcon stRef = do
    dndDa <- drawingAreaNew
    winpop <- windowNewPopup
    windowSetDefaultSize winpop 80 80
    
    containerAdd winpop dndDa
    
    _ <- on dndDa realize $ do
           _ <- dndDa `on` exposeEvent $ io $ do
             st <- readRef stRef
             let dndEb = st ^. gSatDNDSrcCoord
             whenM dndEb (return ()) 
                (\el -> do
                    svgelem <- generateSVGFromEB boardMain boardMod (el ^. _2)
                    drawWindow <- widgetGetDrawWindow dndDa
                    (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize dndDa
                    drawWindowClear drawWindow
                    renderWithDrawable drawWindow (setOperator OperatorOver >> 
                                                   renderPred drawWidth drawHeight svgelem)
                    return ()) >> return True
           return ()
    return $ castToWidget winpop

parseCoord :: String -> (Int,Int)
parseCoord = read

-- | Función principal para el render del board.
configRenderBoard :: SVG -> GuiMonad ()
configRenderBoard svgboard = ask >>= \cnt -> get >>= \s -> io $ do
    let da = cnt ^. gSatDrawArea

    configDrag da cnt s

    _ <- da `on` dragDataReceived $ \_ (x,y) _ _ -> do
           mstr <- selectionDataGetText
           when (isJust mstr) $ io $ do
             squareDst <- getSquare (toEnum x) (toEnum y) da
             (squareSrc,_) <- evalRWST (useG gSatDNDSrcCoord) cnt s
             whenM squareSrc (return ())  $ \el ->  do
                whenM squareDst 
                    (evalGState cnt s $ deleteElemBoardAt (el ^. _1)) $ 
                    \coord -> do
                        evalGState cnt s $ do
                            addNewElem coord (Just (el ^. (_2 . ebPreds))) (el ^. (_2 . ebCnst))
                            addToUndo
                        return ()
                        _ <- evalGState cnt s resetDNDSrcCoord
                        widgetQueueDraw da


    _ <- da `on` exposeEvent $ do
        eRegion <- eventRegion
        _ <- io $ flipEvalRWST cnt s (drawBoard da eRegion)
        return False
    
    return ()
    where
        isJust :: Maybe Text -> Bool
        isJust Nothing = False
        isJust (Just _) = True
        
        drawBoard :: DrawingArea -> Region -> GuiMonad Bool
        drawBoard da exposeRegion = 
            useG gSatBoard >>= \board -> io $ do
            drawWindow              <- widgetGetDrawWindow da
            (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da

            drawWindowClear drawWindow
            renderWithDrawable drawWindow $ do
                let (bWidth, bHeight) = mapPair fromIntegral $ svgGetSize svgboard
                    sideSize = min drawWidth drawHeight - hSpacing
                    xoffset  = (drawWidth - sideSize) / 2
                    yoffset  = (drawHeight - sideSize) / 2
                region exposeRegion
                
                translate xoffset yoffset
                
                save
                scale (sideSize / bWidth) (sideSize / bHeight)
                _ <- svgRender svgboard
                restore
                
                renderElems board sideSize
            return False

renderElems :: Board -> Double -> Render ()
renderElems b sideSize = forM_ (elems b) withElem

    where withElem :: ElemPos -> Render ()
          withElem (Coord x y, e) = do
                   svgelem <- io $ generateSVGFromEB boardMain boardMod e
                   let squareSize = sideSize / realToFrac (size b)
                       (width, height) = mapPair fromIntegral (svgGetSize svgelem)
                   save
                   translate (squareSize * realToFrac x) (squareSize * realToFrac y)
                   scale (squareSize / width) (squareSize / height)
                   _ <- svgRender svgelem
                   restore

getSquare :: Double -> Double -> DrawingArea -> IO (Maybe Coord)
getSquare x y da = do
        (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da
        let sideSize   = min drawWidth drawHeight - hSpacing
            squareSize = sideSize / fromIntegral boardWidth
            xoffset    = (drawWidth - sideSize) / 2
            yoffset    = (drawHeight - sideSize) / 2
        if (x >= xoffset && x < xoffset + sideSize && 
            y >= yoffset && y < yoffset + sideSize)
        then return.Just $ Coord (floor $ (x - xoffset) / squareSize)
                                 (floor $ (y - yoffset) / squareSize)
        else return Nothing


configDrawPieceInBoard :: GuiMonad ()
configDrawPieceInBoard = ask >>= \content -> get >>= \rs -> io $ do
    let da = content ^. gSatDrawArea
    _ <- da `on` buttonPressEvent $ tryEvent $ do
      (x,y) <- eventCoordinates
      click <- eventClick
      button <- eventButton
      io (do 
         square <- getSquare x y da
         flip (maybe (return ())) square $ \ coord -> do
          case (button,click) of
            (LeftButton,SingleClick) -> do
                    evalGState content rs (addElemBoard coord >>
                                           updateFileStatusbarFileChange >>
                                           addToUndo)
                    widgetQueueDraw da
            (RightButton,SingleClick) -> do
                    evalGState content rs (deleteElemBoardAt coord >>
                                           updateFileStatusbarFileChange >>
                                           addToUndo)
                    widgetQueueDraw da
            (LeftButton,DoubleClick) -> do
                    evalGState content rs (entryConstNames coord >>
                                           updateFileStatusbarFileChange >>
                                           addToUndo)
                    return ()
            _ -> return ())
    return ()

deleteElemBoardAt :: Coord -> GuiMonad ()
deleteElemBoardAt coord = useG (gSatBoard . elms) >>= \elemsB ->
                          maybe (return ()) 
                                (updateBoard' elemsB)
                                (lookup coord elemsB)
    where
        updateBoard' :: [ElemPos] -> ElemBoard -> GuiMonad ()
        updateBoard' elemsB toRemove = 
            useG (gSatPieceToAdd .  eaAvails) >>= \avails -> do
              let elems'  = L.delete (coord,toRemove) elemsB
                  avails' = toRemove ^. ebId : avails

              updateStateField (gSatBoard . elms) elems'
              updateStateField (gSatPieceToAdd . eaAvails) avails'

updateDNDSrcCoord :: Coord -> ElemBoard -> GuiMonad ()
updateDNDSrcCoord coord eb = updateStateField gSatDNDSrcCoord (Just (coord,eb))

resetDNDSrcCoord :: GuiMonad ()
resetDNDSrcCoord = updateStateField gSatDNDSrcCoord Nothing


getEBatCoord :: Coord -> GuiMonad (Maybe ElemBoard)
getEBatCoord coord = useG (gSatBoard . to elems) >>= return . lookup coord 
        
entryConstNames :: Coord -> GuiMonad ()
entryConstNames coord = useG gSatBoard >>= \board -> 
                        getEBatCoord coord >>=
                        maybe (return ()) 
                              (addNewTextElem coord (board ^. elms))

addElemBoard :: Coord -> GuiMonad ()
addElemBoard coord = getEBatCoord coord >>= 
                     maybe (addNewElem coord Nothing [])
                           (updateDNDSrcCoord coord)


addNewTextElem :: Coord -> [ElemPos] -> ElemBoard -> GuiMonad ()
addNewTextElem coord elemsB eb = 
    ask >>= \content -> 
    get >>= \stRef ->
    io $ do
    let mainWin = content ^. gSatWindow
    
    win      <- windowNew
    vbox     <- vBoxNew False 0
    entry    <- entryNew
    errLabel <- labelNew (Nothing :: Maybe Text)
    
    set win [ windowWindowPosition := WinPosMouse
            , windowModal          := True
            , windowDecorated      := False
            , windowHasFrame       := False
            , windowTypeHint       := WindowTypeHintPopupMenu
            , widgetCanFocus       := True
            , windowTransientFor   := mainWin
            ]
    
    containerAdd win vbox
    boxPackStart vbox entry    PackNatural 1
    boxPackStart vbox errLabel PackNatural 1
    set errLabel [widgetNoShowAll := True ]

    widgetShowAll win
    
    entrySetText entry $ unwords $ map constName (ebConstant eb)
    
    _ <- on entry keyPressEvent (configEntry win entry errLabel content stRef)
    
    return ()
    where
        configEntry win entry label content stRef = do 
            cNameOk <- do
                    k <- eventKeyName 
                    case k of
                         "Return" -> io $ updateEb entry label content stRef
                         "Escape" -> return True
                         _        -> return False
            when cNameOk (io $ widgetDestroy win >>
                               widgetQueueDraw (content ^. gSatDrawArea))
            return False
       
        updateEb :: Entry -> Label -> GReader -> GStateRef -> IO Bool
        updateEb entry label content stRef = do
            cName <- entryGetText entry
            case parseConstants cName of
                ConstantOk names -> flipEvalRWST content stRef ( do
                                     let elemsB' = map (assigConst names) elemsB
                                     updateStateField (gSatBoard . elms) elemsB'
                                     ) >> return True
                ConstantErr err -> errMsg err
            where
                setMsg :: String -> IO ()
                setMsg msg = widgetSetNoShowAll label False >>
                             labelSetText label msg >> 
                             widgetShowAll label
                errMsg err = setMsg (show err) >> return False

        parseConstants :: String -> ConstantCheck
        parseConstants = foldl (\chk w -> chk <> checkConst w) (ConstantOk []) . words
                                
        checkConst :: String -> ConstantCheck
        checkConst str = lengthOk <> upperOk <> notDuplicated <> ConstantOk [str]
                   where lengthOk = toCheck InvalidLong . (<= 2) $ length str
                         upperOk = toCheck InvalidCase $ all isUpper str
                         notDuplicated = toCheck InvalidDup $ all (checkDoubleConst str) elemsB
                         toCheck err False = ConstantErr err
                         toCheck _ True = ConstantOk []

        checkDoubleConst :: String -> ElemPos -> Bool
        checkDoubleConst cname (_,eb') = (eb' == eb) || not (cname ^. strConst `elem` eb' ^. ebCnst)

        assigConst :: [String] -> ElemPos -> ElemPos
        assigConst cnames e = if coord == e ^. _1 
                              then e & (_2 . ebCnst) .~ map (^. strConst) cnames
                              else e


addNewElem :: Coord -> Maybe [Predicate] -> [Constant] -> GuiMonad ()
addNewElem coord mpreds cnames = do
    cpreds <- useG $ gSatPieceToAdd . eaPreds
    elemsb <- useG $ gSatBoard . elms
    (i,ni,avails) <- newElem
    let eb = ElemBoard i cnames (maybe cpreds id mpreds) 
    updateStateField (gSatBoard . elms) ((coord,eb) : elemsb)
    updateStateField (gSatPieceToAdd . eaMaxId) ni
    updateStateField (gSatPieceToAdd . eaAvails) avails
