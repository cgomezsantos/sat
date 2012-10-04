{-# Language DoAndIfThenElse #-}
-- | Configuración de la lista de símbolos.
module Sat.GUI.SymbolList where

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)

import Data.Text(unpack)

import Lens.Family

import Control.Monad.Trans.State hiding (get,put)
import Control.Monad.Trans.RWS
import Control.Applicative ((<$>))
import Control.Monad (when)

import Sat.GUI.GState

type SymItem = String

scrollInc :: Double
scrollInc = 10.0

scrollDec :: Double
scrollDec = - scrollInc

listSymbols :: IO (ListStore SymItem)
listSymbols = listStoreNew ["Acá irían los símbolos para escribir expresiones."]

configSymFrameButton :: GuiMonad ()
configSymFrameButton = do
                content <-  ask
                let sf          = content ^. (gSatSymbolList . gSymFrame)
                let sfButton    = content ^. (gSatToolbar . symFrameB)
                
                active <- io $ toggleToolButtonGetActive sfButton
                if active 
                   then io $ widgetShowAll sf
                   else io $ widgetHideAll sf

configSymbolList :: GuiMonad ()
configSymbolList = do
                content <-  ask
                s <- get
                let sf      = content ^. (gSatSymbolList . gSymFrame)
                let iv      = content ^. (gSatSymbolList . gSymIconView)
                let goLB    = content ^. (gSatSymbolList . gGoLeftBox)
                let goRB    = content ^. (gSatSymbolList . gGoRightBox)
                let scrollW = content ^. (gSatSymbolList . gScrollW)
                
                list <- io listSymbols
                io $ setupScrolledWindowSymbolList scrollW goLB goRB s
                io $ setupSymbolList iv list
                io $ widgetHideAll sf
                
                return ()

-- | La configuración de la lista de símbolos propiamente hablando.
setupSymbolList :: IconView -> ListStore SymItem -> IO (ListStore SymItem)
setupSymbolList iv list = 
     listStoreGetSize list >>= \listSize ->
     return (makeColumnIdString 1) >>= \scol ->
     return (makeColumnIdPixbuf (-1)) >>= \pcol ->
     iconViewSetTextColumn iv scol >>
     iconViewSetPixbufColumn iv pcol >>
     customStoreSetColumn list scol id >>
     set iv [ iconViewModel := Just list
            , iconViewPixbufColumn := pcol
            , iconViewTextColumn := scol
            , iconViewColumns := listSize
            , iconViewRowSpacing := 0
            , iconViewMargin := 0
            , iconViewSelectionMode := SelectionSingle
            ] >>
     widgetShowAll iv >>
     return list

setupScrolledWindowSymbolList :: ScrolledWindow -> HBox -> HBox -> GStateRef -> IO ()
setupScrolledWindowSymbolList sw goLb goRb s = do
            goR <- makeScrollArrow goRb stockGoForward
            goL <- makeScrollArrow goLb stockGoBack
            (Just  swslH) <- scrolledWindowGetHScrollbar sw
            adj <- rangeGetAdjustment swslH
            setupScrollWithArrow adj goR scrollInc
            setupScrollWithArrow adj goL scrollDec
            widgetSetChildVisible swslH False
            widgetHide swslH
            widgetShowAll goLb
            widgetShowAll goRb

setupScrollWithArrow :: Adjustment -> Button -> Double -> IO (ConnectId Button)
setupScrollWithArrow adj go inc = 
                go `on` buttonPressEvent $ tryEvent $ do
                        val      <- io $ adjustmentGetValue adj
                        upper    <- io $ adjustmentGetUpper adj
                        pageSize <- io $ adjustmentGetPageSize adj
                        when (upper - pageSize > val + inc) $ 
                             io $ adjustmentSetValue adj (val + inc)

makeScrollArrow :: HBox -> StockId -> IO Button
makeScrollArrow box si = do
                        symGo <- buttonNew
                        
                        buttonSetRelief symGo ReliefNone
                        
                        arrow <- imageNewFromStock si IconSizeMenu
                        
                        buttonSetImage symGo arrow
                        
                        boxPackStart box symGo PackNatural 0
                        return symGo
