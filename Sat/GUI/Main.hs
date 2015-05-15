{-# Language DoAndIfThenElse #-}
-- | Módulo principal de la interfaz.
module Main where

import Graphics.UI.Gtk 

import System.Glib
import System.Environment

import Sat.GUI.Gui

import Data.Version
import Paths_sat

wikipage :: String
wikipage = "https://github.com/manugunther/sat/wiki/Manual-de-uso"

main :: IO ()
main = do 
    args <- getArgs

    if (length args > 0) then
      case head args of
        "--help" -> putStrLn $ "Para ver cómo usar el programa consultá: " ++ wikipage
        "--version" -> putStrLn $ "Versión: " ++ showVersion version 
        _ -> gui
    else gui
        
gui :: IO ()
gui = do
    _ <- initGUI
    _ <- setProgramName "sat"
    _ <- setApplicationName "sat"
    
    xml <- builderNew
    uifn <- getDataFileName "sat.ui"
    _ <- builderAddFromFile xml uifn
    
    _ <- mainSatGui xml
    
    mainGUI
