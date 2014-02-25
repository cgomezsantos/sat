-- | Módulo principal de la interfaz.
module Main where

import Graphics.UI.Gtk 

import System.Glib

import Sat.GUI.Gui

import Paths_sat

main :: IO ()
main = do 
    _ <- initGUI
    _ <- setProgramName "sat"
    _ <- setApplicationName "sat"
    
    xml <- builderNew
    uifn <- getDataFileName "sat.ui"
    _ <- builderAddFromFile xml uifn
    
    _ <- mainSatGui xml
    
    mainGUI
