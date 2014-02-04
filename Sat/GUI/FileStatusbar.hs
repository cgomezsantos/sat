module Sat.GUI.FileStatusbar where

import Control.Lens

import Graphics.UI.Gtk hiding ( eventRegion, eventKeyName, get)

import Control.Monad.Trans.RWS (ask)

import Sat.GUI.GState

fileWithoutName :: String
fileWithoutName = "mundo.sat"

updateFileStatusbar :: String -> GuiMonad ()
updateFileStatusbar msg = ask >>= \cnt -> io $ do
    let fileSb = cnt ^. gSatFileStatusbar
    
    ctx <- statusbarGetContextId fileSb "Line"
    statusbarPop fileSb ctx
    _ <- statusbarPush fileSb ctx ("Archivo: " ++ msg)
    return ()

updateFileStatusbarNewFile :: GuiMonad ()
updateFileStatusbarNewFile = updateFileStatusbar fileWithoutName

updateFileStatusbarFileSave :: GuiMonad ()
updateFileStatusbarFileSave = getGState >>= \st -> do
    let mfp    = st ^. gSatFile
        msg    = maybe fileWithoutName (^. gname) mfp
    
    updateFileStatusbar $ msg

updateFileStatusbarFileChange :: GuiMonad ()
updateFileStatusbarFileChange = getGState >>= \st -> do
    let mfp    = st ^. gSatFile
        msg    = maybe fileWithoutName (^. gname) mfp
    
    updateFileStatusbar $ msg ++ "*"
