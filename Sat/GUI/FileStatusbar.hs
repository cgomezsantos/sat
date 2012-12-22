module Sat.GUI.FileStatusbar where

import Lens.Family

import Graphics.UI.Gtk hiding ( eventRegion, eventKeyName, get)

import Control.Monad.Trans.RWS (ask)

import Sat.GUI.GState

fileWithoutName :: String
fileWithoutName = "Nuevo archivo.sat"

updateFileStatusbar :: String -> GuiMonad ()
updateFileStatusbar msg = ask >>= \cnt -> io $ do
    let fileSb = cnt ^. gSatFileStatusbar
    
    ctx <- statusbarGetContextId fileSb "Line"
    statusbarPop fileSb ctx
    statusbarPush fileSb ctx ("File: " ++ msg)
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
