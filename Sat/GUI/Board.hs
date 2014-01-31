{-# Language DoAndIfThenElse #-}
-- | Renderiza el board para la interfaz en base a un archivo SVG.
module Sat.GUI.Board where

import Control.Monad 
import Control.Monad.Trans.RWS (ask,evalRWST,get,RWST)

import Lens.Family

import Data.Maybe
import Data.Char (isUpper)
import qualified Data.List as L
import Data.Reference (readRef)

import Graphics.UI.Gtk hiding ( eventRegion, eventKeyName, get)
import Graphics.UI.Gtk.Gdk.Events hiding ( eventButton, eventClick)
import Graphics.Rendering.Cairo hiding (x,y,width,height)
import Graphics.Rendering.Cairo.SVG (SVG,svgGetSize,svgRender,svgGetSize,svgRender)



import Sat.Core
import Sat.VisualModel (interpPreds)
import Sat.VisualModels.FiguresBoard

import Sat.GUI.SVG hiding ((<>))
import Sat.GUI.GState
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
_ <> ConstantErr e = ConstantErr e
ConstantOk cs <> ConstantOk cs' = ConstantOk $ cs ++ cs'

instance Show ErrConstantCheck where
    show InvalidLong = "Los nombres de constantes deben tener a lo sumo dos letras"
    show InvalidCase = "Los nombres de constantes deben ser en mayúscula"
    show InvalidDup  = "Constante interpretada por otro elemento"


whenM :: Maybe a -> b -> (a -> b) -> b
whenM may dont does = maybe dont does may

hSpacing :: Double
hSpacing = 20

flipEvalRWST :: Monad m => r -> s -> RWST r w s m a -> m (a, w)
flipEvalRWST r s rwst = evalRWST rwst r s

configDrag :: DrawingArea -> GReader -> GStateRef -> IO ()
configDrag da cnt stRef = do
        dragSourceSet da [Button1] [ActionCopy]
        dragSourceSetIconStock da stockAdd

        dragSourceAddTextTargets da

        dragDestSet da [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
        dragDestAddTextTargets da
        
        winpop <- configDNDIcon stRef
        
        _ <- da `on` dragDataGet $ \_ _ _ -> selectionDataSetText "dnd" >> return ()
        
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
                            (\(_,colSrc,rowSrc) -> deleteElemBoardAt colSrc rowSrc)
            return ()

configDNDIcon :: GStateRef -> IO Widget
configDNDIcon stRef = do
    dndDa <- drawingAreaNew
    winpop <- windowNewPopup
    windowSetDefaultSize winpop 80 80
    
    containerAdd winpop dndDa
    
    _ <- on dndDa realize $ do
           _ <- dndDa `onExpose` (\_ -> do
             st <- readRef stRef
             let dndEb = st ^. gSatDNDSrcCoord
             whenM dndEb (return ()) 
                (\(eb,_,_) -> do
                    svgelem <- generateSVGFromEB boardMain boardMod eb
                    drawWindow <- widgetGetDrawWindow dndDa
                    (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize dndDa
                    drawWindowClear drawWindow
                    renderWithDrawable drawWindow (setOperator OperatorOver >> 
                                                   renderPred drawWidth drawHeight svgelem)
                    return ()) >> return True)
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
           whenM  mstr (return ()) $ \_ -> io $ do
             squareDst <- getSquare (toEnum x) (toEnum y) da
             (st,_) <- evalRWST getGState cnt s
             let squareSrc = st ^. gSatDNDSrcCoord
             whenM squareSrc (return ())  $ \(eb,colSrc,rowSrc) ->  do
                whenM squareDst 
                    (evalGState cnt s $ deleteElemBoardAt colSrc rowSrc) $ 
                    \(col,row) -> do
                        let preds  = interpPreds eb
                            cName = ebConstant eb
                        evalGState cnt s $ do
                            st' <- getGState
                            let board  = st' ^. gSatBoard
                                elemsB = elems board
                            (board',i,avails) <- addNewElem (Coord col row) 
                                                            (Just preds) 
                                                            cName elemsB board
                            updateBoardState avails i board'
                            addToUndo
                        return ()
                        _ <- evalGState cnt s resetDNDSrcCoord
                        widgetQueueDraw da


    _ <- da `onExpose` \expose ->
        flipEvalRWST cnt s (drawBoard da expose) >> 
        return False
    
    return ()
    where
        drawBoard :: DrawingArea -> Event -> GuiMonad Bool
        drawBoard da expose = getGState >>= \st -> io $ do
            let exposeRegion = eventRegion expose
                board = st ^. gSatBoard
            drawWindow              <- widgetGetDrawWindow da
            (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da

            drawWindowClear drawWindow
            renderWithDrawable drawWindow $ do
                let (boardWidth, boardHeight) = mapPair fromIntegral $ svgGetSize svgboard
                    sideSize = min drawWidth drawHeight - hSpacing
                    xoffset  = (drawWidth - sideSize) / 2
                    yoffset  = (drawHeight - sideSize) / 2
                region exposeRegion
                
                translate xoffset yoffset
                
                save
                scale (sideSize / boardWidth) (sideSize / boardHeight)
                _ <- svgRender svgboard
                restore
                
                renderElems board sideSize
            return False

renderElems :: Board -> Double -> Render ()
renderElems b sideSize = forM_ (elems b) withElem

    where withElem :: (Coord,ElemBoard) -> Render ()
          withElem (Coord x y, e) = do
                   svgelem <- io $ generateSVGFromEB boardMain boardMod e
                   let squareSize = sideSize / realToFrac (size b)
                       (width, height) = mapPair fromIntegral (svgGetSize svgelem)
                   save
                   translate (squareSize * realToFrac x) (squareSize * realToFrac y)
                   scale (squareSize / width) (squareSize / height)
                   _ <- svgRender svgelem
                   restore

getSquare :: Double -> Double -> DrawingArea -> IO (Maybe (Int,Int))
getSquare x y da = do
        (drawWidth, drawHeight) <- liftM (mapPair fromIntegral) $ widgetGetSize da
        let sideSize   = min drawWidth drawHeight - hSpacing
            squareSize = sideSize / 8
            xoffset    = (drawWidth - sideSize) / 2
            yoffset    = (drawHeight - sideSize) / 2
        if (x >= xoffset && x < xoffset + sideSize && 
            y >= yoffset && y < yoffset + sideSize)
        then return $ Just ( floor ((x - xoffset) / squareSize)
                           , floor ((y - yoffset) / squareSize))
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
         flip (maybe (return ())) square $ \ (colx,rowy) -> do
          case (button,click) of
            (LeftButton,SingleClick) -> do
                    evalGState content rs (handleLeftSingleClick colx rowy >>
                                           updateFileStatusbarFileChange >>
                                           addToUndo)
                    widgetQueueDraw da
            (RightButton,SingleClick) -> do
                    evalGState content rs (deleteElemBoardAt colx rowy >>
                                           updateFileStatusbarFileChange >>
                                           addToUndo)
                    widgetQueueDraw da
            (LeftButton,DoubleClick) -> do
                    evalGState content rs (handleLeftDoubleClick colx rowy >>
                                           updateFileStatusbarFileChange >>
                                           addToUndo)
                    return ()
            _ -> return ())
    return ()

deleteElemBoardAt :: Int -> Int -> GuiMonad ()
deleteElemBoardAt colx rowy = do
    st <- getGState
    let board = st ^. gSatBoard
        elemsB = elems board 
        
        cords = Coord colx rowy
        elemToDelete = lookup cords elemsB
    
    when (isJust elemToDelete) (updateBoardState' cords board (fromJust elemToDelete) elemsB)
    where
        updateBoardState' :: Coord -> Board -> ElemBoard -> 
                            [(Coord,ElemBoard)] -> GuiMonad ()
        updateBoardState' cords board elemToDelete elemsB = 
            getGState >>= \st -> do
            let avails  = st ^. (gSatPieceToAdd . eaAvails)
                elems'  = L.delete (cords,elemToDelete) elemsB
                avails' = uElemb elemToDelete : avails
                
            updateGState ((<~) gSatBoard board{elems = elems'})
            updateGState ((<~) (gSatPieceToAdd . eaAvails) avails')

updateDNDSrcCoord :: ElemBoard -> Int -> Int -> GuiMonad ()
updateDNDSrcCoord eb col row = updateGState (gSatDNDSrcCoord <~ (Just (eb,col,row)))

resetDNDSrcCoord :: GuiMonad ()
resetDNDSrcCoord = updateGState (gSatDNDSrcCoord <~ Nothing)


getEBatCoord :: Int -> Int -> GuiMonad (Maybe ElemBoard)
getEBatCoord col row = do 
            let coord = Coord col row
            
            st <- getGState
            let board  = st ^. gSatBoard
                elemsB = elems board
            return $ lookup coord elemsB
        
handleLeftDoubleClick :: Int -> Int -> GuiMonad ()
handleLeftDoubleClick colx rowy = do
            st <- getGState
            let board  = st ^. gSatBoard
                elemsB = elems board

            let coord = Coord colx rowy
            meb <- getEBatCoord colx rowy
            case meb of
                 Just eb -> addNewTextElem coord eb elemsB board
                 Nothing -> return ()

handleLeftSingleClick :: Int -> Int -> GuiMonad ()
handleLeftSingleClick colx rowy = do
           
            st <- getGState
            let board  = st ^. gSatBoard
                elemsB = elems board

            let coord = Coord colx rowy
            meb <- getEBatCoord colx rowy
            case meb of
                Just eb -> updateDNDSrcCoord eb colx rowy
                Nothing -> addNewElem coord Nothing [] elemsB board >>= 
                           \(board',i,avails) -> updateBoardState avails i board'


addNewTextElem :: Coord -> ElemBoard -> [(Coord,ElemBoard)] -> 
                  Board -> GuiMonad ()
addNewTextElem coord eb elemsB board = 
    ask >>= \content -> get >>= \stRef ->
    io $ do
    let mainWin = content ^. gSatWindow
    
    win      <- windowNew
    vbox     <- vBoxNew False 0
    entry    <- entryNew
    errLabel <- labelNew Nothing
    
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
    widgetSetNoShowAll errLabel True
    widgetShowAll win
    
    entrySetText entry $ unwords $ map constName (ebConstant eb)
    
    _ <- onKeyPress entry (configEntry win entry errLabel content stRef)
    
    return ()
    where
        configEntry :: Window -> Entry -> Label -> GReader -> 
                       GStateRef -> Event -> IO Bool
        configEntry win entry label content stRef e = do
            cNameOk <- checkEvent
            if cNameOk
                then widgetDestroy win >>
                    widgetQueueDraw (content ^. gSatDrawArea) >>
                    return False
                else return False
            where
                checkEvent :: IO Bool
                checkEvent = case eventKeyName e of
                                "Return" -> updateEb entry label content stRef
                                "Escape" -> return True
                                _        -> return False
       
        updateEb :: Entry -> Label -> GReader -> GStateRef -> IO Bool
        updateEb entry label content stRef = do
            cName <- entryGetText entry
            case parseConstants cName of
                ConstantOk names -> flipEvalRWST content stRef ( do
                                     let elemsB' = map (assigConst names) elemsB
                                         board'  = board {elems = elemsB'}
                                     updateGState ((<~) gSatBoard board')
                                     ) >> return True
                ConstantErr err -> errMsg err
            where
                setMsg :: String -> IO ()
                setMsg msg = widgetSetNoShowAll label False >>
                             labelSetText label msg >> 
                             widgetShowAll label
                errMsg err = setMsg (show err) >> return False


        parseConstants :: String -> ConstantCheck
        parseConstants ws = if notDups cts
                            then foldl (\chk w -> chk <> checkConst w) (ConstantOk []) cts
                            else ConstantErr InvalidDup
           where cts = words ws
                 notDups :: Eq a => [a] -> Bool
                 notDups [] = True
                 notDups [x] = True
                 notDups (x:(y:xs)) | x == y = False
                                    | x /= y = notDups (y:xs)                                  
        checkConst :: String -> ConstantCheck
        checkConst str = lengthOk str <> upperOk str <> notDuplicated str <> ConstantOk [str]
           where lengthOk = toCheck InvalidLong . (<= 2) . length 
                 upperOk = toCheck InvalidCase . all isUpper
                 notDuplicated str' = toCheck InvalidDup $ all (checkDoubleConst str') elemsB
                 toCheck err False = ConstantErr err
                 toCheck _ True = ConstantOk []

        checkDoubleConst :: String -> (Coord,ElemBoard) -> Bool
        checkDoubleConst str (_,eb') = (eb' == eb) || not (elem (Constant str) (ebConstant eb'))

        assigConst :: [String] -> (Coord,ElemBoard) -> 
                        (Coord,ElemBoard)
        assigConst cnames (coord',eb') =
            if coord == coord'
            then (coord',eb' {ebConstant = map Constant cnames })
            else (coord',eb')


addNewElem :: Coord -> Maybe [Predicate] -> [Constant] -> [(Coord,ElemBoard)] -> Board -> 
              GuiMonad (Board,Univ,[Univ])
addNewElem coord mpreds cnames elemsB board = do
    st <- getGState
    let preds  = maybe (st ^. (gSatPieceToAdd . eaPreds)) id mpreds

    (eb,i,avails) <- newElem coord preds cnames
    let e = (coord,eb)
    
    return (board {elems = e : elemsB},i,avails)

newElem :: Coord -> [Predicate] -> [Constant] -> GuiMonad (ElemBoard,Univ,[Univ])
newElem _ preds cnames = do 
    st <- getGState
    let avails = st ^. (gSatPieceToAdd . eaAvails)
        i      = st ^. (gSatPieceToAdd . eaMaxId)
    
    return $ 
        if null avails
        then (ElemBoard (i + 1) cnames preds,i + 1,avails)
        else (ElemBoard (head avails) cnames preds,i,tail avails)



updateBoardState :: [Univ] -> Univ -> Board -> GuiMonad ()
updateBoardState avails i board = do updateGState ((<~) gSatBoard board)
                                     updateGState ((<~) (gSatPieceToAdd . eaMaxId) i)
                                     updateGState ((<~) (gSatPieceToAdd . eaAvails) avails)
