module Main where

import Display

import Control.Monad

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as E
import qualified Graphics.UI.Threepenny.Attributes as A

main :: IO ()
main = do

    let settings = defaultSettings

    -- create the start-image if neccessary
    verifyStartImageFile settings

    putStrLn "starting local server..."
    startGUI defaultConfig
        { tpPort = 10000
        , tpStatic = Just "./content"
        } (setup settings)

defaultSettings :: Settings
defaultSettings = Settings defaultRes defaultZoom defaultSteps
    where
    defaultRes     = Size 1024 711
    defaultSteps   = 1024
    defaultZoom    = 4

setup :: Settings -> Window -> UI ()
setup settings w = void $ do
        E.addStyleSheet w "styles.css"
        return w # set title "Mandelbrot generator "

        md <- mandelbrotDisplay settings
        output <- E.div # set A.class_ "output" #+ [element md]

        statusText <- string promptClick #
           sink text (statusForRendering <$> isRendering md)

        mousePosText <- string "[-]" #
            sink text (statusForMouseCoords <$> mousePos md)

        status <- E.ul # set A.class_ "status" #+ [E.li #+ [element statusText], E.li #+ [element mousePosText]]

        getBody w #+ [element output, element status]

statusForRendering :: Bool -> String
statusForRendering True  = waitMessage
statusForRendering False = promptClick

statusForMouseCoords :: Compl -> String
statusForMouseCoords c = "[" ++ show c ++ "]"

promptClick :: String
promptClick = "click anywhere to zoom in to this point"

waitMessage :: String
waitMessage = "rendering please wait ..."