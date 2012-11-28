{-# Language DoAndIfThenElse #-}
-- | Renderiza el board para la interfaz en base a un archivo SVG.
module Sat.GUI.Board where

import Control.Monad 
import Control.Monad.Trans.RWS (ask,evalRWST,get,RWST)

import Lens.Family

import Data.Maybe
import Data.Char (isUpper)
import qualified Data.List as L
import qualified Data.Map as M

import Graphics.UI.Gtk hiding ( eventRegion, eventKeyName, get)
import Graphics.UI.Gtk.Gdk.Events hiding ( eventButton, eventClick)
import Graphics.UI.Gtk.General.Selection
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Sat.Core
import Sat.VisualModel (visualToModel,interpPreds)
import Sat.VisualModels.FiguresBoard
import Sat.Signatures.Figures(arriba,izquierda,abajo,derecha,rojo,triangulo)

import Sat.GUI.SVG
import Sat.GUI.SVGBoard
import Sat.GUI.GState
import Sat.GUI.IconTable
import Sat.GUI.FigureList
import Sat.GUI.UndoRedo

whenM may dont does = maybe dont does may

hSpacing :: Double
hSpacing = 20

flipEvalRWST :: Monad m => r -> s -> RWST r w s m a -> m (a, w)
flipEvalRWST r s rwst = evalRWST rwst r s

configDrag :: DrawingArea -> IO ()
configDrag da = do
        dragSourceSet da [Button1] [ActionCopy]
        dragSourceSetIconStock da stockAdd

        dragSourceAddTextTargets da

        dragDestSet da [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
        dragDestAddTextTargets da
        
        da `on` dragDataGet $ \dc ifd ts -> do
               selectionDataSetText "dnd"
               return ()
        return ()

parseCoord :: String -> (Int,Int)
parseCoord = read

-- | Función principal para el render del board.
configRenderBoard :: SVG -> GuiMonad ()
configRenderBoard svgboard = ask >>= \cnt -> get >>= \s -> io $ do
    let da = cnt ^. gSatDrawArea

    configDrag da

    da `on` dragDataReceived $ \dc (x,y) id ts -> do
          mstr <- selectionDataGetText
          whenM  mstr (return ()) $ \str -> io $ do
            squareDst <- getSquare (toEnum x) (toEnum y) da
            (st,_) <- evalRWST getGState cnt s
            let squareSrc = st ^. gSatDNDSrcCoord
            whenM squareSrc (return ())  $ \(colSrc,rowSrc) ->  do
                meb <- evalGState cnt s (getEBatCoord colSrc rowSrc) 
                whenM meb (return ()) $ \ eb -> do
                  let board  = st ^. gSatBoard
                      elemsB = elems board
                  whenM squareDst (evalGState cnt s $ deleteElemBoardAt colSrc rowSrc) $ \(col,row) -> do
                    let preds  = interpPreds eb
                        conName = ebConstant eb
                    evalGState cnt s $ do
                      deleteElemBoardAt colSrc rowSrc
                      st <- getGState
                      let board  = st ^. gSatBoard
                          elemsB = elems board
                      (board,i,avails) <- addNewElem (Coord col row) (Just preds) conName elemsB board
                      updateBoardState avails i board
                      addToUndo
                    return ()
                  evalGState cnt s resetDNDSrcCoord
                  widgetQueueDraw da


    da `onExpose` \expose ->
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
                svgRender svgboard
                restore
                
                renderElems board sideSize
            return False

renderElems :: Board -> Double -> Render ()
renderElems b sideSize = 
    forM_ (elems b) $ \(Coord x y, e) -> do
        svgelem <- io $ generateSVGFromEB boardMain boardMod e
        let squareSize = sideSize / realToFrac (size b)
            (width, height) = mapPair fromIntegral (svgGetSize svgelem)
        
        save
        translate (squareSize * fromIntegral (toEnum x)) (squareSize * fromIntegral (toEnum y))
        scale (squareSize / width) (squareSize / height)
        svgRender svgelem
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


configDrawPieceInBoard :: SVG -> GuiMonad ()
configDrawPieceInBoard b = ask >>= \content -> get >>= \rs -> io $ do
    let da = content ^. gSatDrawArea
    da `on` buttonPressEvent $ tryEvent $ do
      (x,y) <- eventCoordinates
      click <- eventClick
      button <- eventButton
      io (do 
         square <- getSquare x y da
         flip (maybe (return ())) square $ \ (colx,rowy) -> do
          case (button,click) of
            (LeftButton,DoubleClick) -> do
                    evalGState content rs (handleLeftClick colx rowy >>
                                           addToUndo)
                    widgetQueueDraw da
            (RightButton,SingleClick) -> do
                    evalGState content rs (deleteElemBoardAt colx rowy >>
                                           addToUndo)
                    widgetQueueDraw da
            (LeftButton,SingleClick) -> do
                    evalGState content rs (updateDNDSrcCoord colx rowy)
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
        i      = st ^. (gSatPieceToAdd . eaMaxId)
    
    when (isJust elemToDelete) (updateBoardState cords board (fromJust elemToDelete) elemsB)
    io $ putStrLn $ "Borrando. MaxId = " ++ show i
    where
        updateBoardState :: Coord -> Board -> ElemBoard -> 
                            [(Coord,ElemBoard)] -> GuiMonad ()
        updateBoardState cords board elemToDelete elemsB = 
            ask >>= \content -> getGState >>= \st -> do
            let avails  = st ^. (gSatPieceToAdd . eaAvails)
                elems'  = L.delete (cords,elemToDelete) elemsB
                avails' = uElemb elemToDelete : avails
                iconEdit = content ^. gSatMainStatusbar
                
            updateGState ((<~) gSatBoard board{elems = elems'})
            updateGState ((<~) (gSatPieceToAdd . eaAvails) avails')
            makeModelButtonWarning

updateDNDSrcCoord col row = updateGState (gSatDNDSrcCoord <~ (Just (col,row)))
resetDNDSrcCoord = updateGState (gSatDNDSrcCoord <~ Nothing)


getEBatCoord :: Int -> Int -> GuiMonad (Maybe ElemBoard)
getEBatCoord col row = do 
            let coord = Coord col row
            
            st <- getGState
            let board  = st ^. gSatBoard
                elemsB = elems board
            meb <- return $ lookup coord elemsB
            return meb
        
handleLeftClick :: Int -> Int -> GuiMonad ()
handleLeftClick colx rowy = do
           
            st <- getGState
            let board  = st ^. gSatBoard
                elemsB = elems board

            let coord = Coord colx rowy
            meb <- getEBatCoord colx rowy
            case meb of
                Just eb -> do 
                       addNewTextElem coord eb elemsB board
                Nothing -> addNewElem coord Nothing Nothing elemsB board >>= \(board,i,avails) ->
                           updateBoardState avails i board


addNewTextElem :: Coord -> ElemBoard -> [(Coord,ElemBoard)] -> 
                  Board -> GuiMonad ()
addNewTextElem coord eb elemsB board = 
    ask >>= \content -> getGState >>= \st -> get >>= \stRef ->
    io $ do
    let avails = st ^. (gSatPieceToAdd . eaAvails)
        i      = st ^. (gSatPieceToAdd . eaMaxId)
        mainWin = content ^. gSatWindow
    
    win   <- windowNew
    entry <- entryNew
    
    set win [ windowWindowPosition := WinPosMouse
            , windowModal          := True
            , windowDecorated      := False
            , windowHasFrame       := False
            , windowTypeHint       := WindowTypeHintPopupMenu
            , widgetCanFocus       := True
            , windowTransientFor   := mainWin
            ]
    
    containerAdd win entry
    widgetShowAll win
    
    entrySetText entry $ maybe "" constName (ebConstant eb)
    
    onKeyPress entry (configEntry win entry content stRef)
    
    return ()
    where
      configEntry :: Window -> Entry -> GReader -> 
                     GStateRef -> Event -> IO Bool
      configEntry win entry content stRef e = do
          cNameOk <- if eventKeyName e == "Return"
                          then updateEb entry content stRef
                          else return False
          if cNameOk
             then widgetDestroy win >>
                  widgetQueueDraw (content ^. gSatDrawArea) >>
                  evalRWST makeModelButtonWarning content stRef >>
                  return False
             else return False
      updateEb :: Entry -> GReader -> GStateRef -> IO Bool
      updateEb entry content stRef = do
          cName <- entryGetText entry
          if checkConstantName cName
             then flipEvalRWST content stRef (do
                      let elemsB' = map (assigConst cName) elemsB
                          board'  = board {elems = elemsB'}
                      updateGState ((<~) gSatBoard board')
                      ) >> return True
             else return False

      checkConstantName :: String -> Bool
      checkConstantName str =  length str <= 2 
                            && all isUpper str 
                            && all (checkDoubleConst str) elemsB

      checkDoubleConst :: String -> (Coord,ElemBoard) -> Bool
      checkDoubleConst str (_,eb') = (eb' == eb) ||
                                     (Just (Constant str) /=  ebConstant eb')

      assigConst :: String -> (Coord,ElemBoard) -> 
                    (Coord,ElemBoard)
      assigConst cName (coord',eb') =
          if coord == coord'
          then (coord',eb' {ebConstant = named })
          else (coord',eb')
              where named = if null cName then Nothing else Just $ Constant cName


addNewElem :: Coord -> Maybe [Predicate] -> Maybe Constant -> [(Coord,ElemBoard)] -> Board -> 
              GuiMonad (Board,Univ,[Univ])
addNewElem coord mpreds cname elemsB board = do
    st <- getGState
    let preds  = maybe (st ^. (gSatPieceToAdd . eaPreds)) id mpreds

    (eb,i,avails) <- newElem coord preds cname
    io $ putStrLn $ "Agregando elemento. MaxId = " ++ show i
    let e = (coord,eb)
    
    return (board {elems = e : elemsB},i,avails)

newElem :: Coord -> [Predicate] -> Maybe Constant -> GuiMonad (ElemBoard,Univ,[Univ])
newElem coord preds mname = do
    st <- getGState
    let avails = st ^. (gSatPieceToAdd . eaAvails)
        i      = st ^. (gSatPieceToAdd . eaMaxId)
    
    return $ 
        if null avails
        then (ElemBoard (i + 1) mname preds,i + 1,avails)
        else (ElemBoard (head avails) mname preds,i,tail avails)



updateBoardState :: [Univ] -> Univ -> Board -> GuiMonad ()
updateBoardState avails i board = ask >>= \content -> do 
    let iconEdit = content ^. gSatMainStatusbar
    
    updateGState ((<~) gSatBoard board)
    updateGState ((<~) (gSatPieceToAdd . eaMaxId) i)
    updateGState ((<~) (gSatPieceToAdd . eaAvails) avails)
    makeModelButtonWarning



makeModelFromBoard :: GuiMonad ()
makeModelFromBoard = getGState >>= \st -> do
    let visual = st ^. gSatBoard
        model  = visualToModel visual
        
    updateGState ((<~) gSatModel model)
    makeModelButtonOk
--     io $ putStrLn $ "arriba "++ show (M.lookup arriba (interpRelations model))
--     io $ putStrLn $ "izquierda "++ show (M.lookup izquierda (interpRelations model))
--     io $ putStrLn $ "abajo "++ show (M.lookup abajo (interpRelations model))
--     io $ putStrLn $ "derecha "++ show (M.lookup derecha (interpRelations model))
--     io $ putStrLn $ "rojo "++ show (M.lookup rojo (interpPredicates model))
--     io $ putStrLn $ "triangulo "++ show (M.lookup triangulo (interpPredicates model))
