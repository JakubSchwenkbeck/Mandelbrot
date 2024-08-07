--| Display handles all the Pixelplacement and the PNG File creation 
module Display 
    ( Settings (..)
    , MandelbrotDisplay (..)
    , Complex (..)
    , PictureSize (..)
    , PictureCoords (..)
    , mandelbrotDisplay
    , verifyStartImageFile
    ) where

--Imports:

--| Importing the selfwritten Mandelbrot functions
import Mandelbrot

--| Importing concurenncy and file util
import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, getChanContents, writeChan)
import System.Directory (doesFileExist)
--| Importing the Threepenny GUI 
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Attributes as Attr
import qualified Graphics.UI.Threepenny.Events as Ev
import Graphics.UI.Threepenny.Core

 --| init settings
data Settings = Settings
    { resolution  :: PictureSize
    , zoom        :: Double
    , renderSteps :: Steps}
    
--| init struct for Display
data MandelbrotDisplay = MandelbrotDisplay
    { view        :: Behavior ViewWindow
    , isRendering :: Behavior Bool
    , mousePos    :: Behavior Complex
    , visual      :: Element
    }

--| Init structs with values
instance Widget MandelbrotDisplay where
    getElement = visual    

startView :: ViewWindow
startView          = View (C (-2.4) 1.2) (C 1.1 (-1.4))

startImageName :: String
startImageName = "start.png"

renderImageName :: String
renderImageName = "mandelbrot.png"

localPath :: String -> FilePath
localPath name = "./static/" ++ name

clientPath :: String -> String
clientPath name = "/static/" ++ name

--| create a UI from the Settings and MandelbrotDisplay struct
mandelbrotDisplay :: Settings -> UI MandelbrotDisplay
mandelbrotDisplay settings = do
    img <- mkImage

    --| we want to run the rendering in another thread
    (eDone, run) <- liftIO newAsync

    let
        eMousedown = Ev.mousedown img

        getPos (ptX, ptY) vw = project vw (resolution settings) (Coords  ptX ptY)

        makeViewChange (ptX, ptY) vw = zoomTo (zoom settings) cent vw
            where cent = getPos (ptX, ptY) vw

        render vw = liftIO . run $ renderView settings vw (localPath renderImageName)

        setImage () = element img # set Attr.src (clientPath renderImageName)

    --| define behaviours (start rendering on mousedown, stop on end of rendering)
    rendering <- stepper False $ unionWith (||) (const False <$> eDone) (const True <$> eMousedown)
    --| change the view based on clicked position if not currently rendering
    let eMousedownInt = fmap (\(x, y) -> (floor x, floor y)) eMousedown
    vw <- accumB startView $ makeViewChange <$> whenE (not <$> rendering) eMousedownInt

    --| get the Complex-coordinates for the mousepos
    mPos      <- stepper (0,0) $ Ev.mousemove img
    let 
    
        cPos =  (getPos <$> (fmap (\(x, y) -> (floor x, floor y)) mPos)) <*> vw


    --| hook up side-effects
    onEvent   eDone     setImage
    onChanges vw        render

    return $ (MandelbrotDisplay vw rendering cPos img)

renderView :: Settings -> ViewWindow -> FilePath -> IO()
renderView settings vw = createImageInParallel vw (renderSteps settings) (resolution settings)

mkImage :: UI Element
mkImage = do
    img <- UI.image #. "mandelImage"
    element img # set Attr.src (clientPath startImageName)

-- | Perform an 'IO' operation asynchronously.
-- The 'Event' indicates when the action has finished.
-- (shamelessly copy&pasted from Heinrich Apfelmus)
newAsync :: IO (Event (), Handler (IO ()))
newAsync = do
    (eDone, done) <- newEvent
    
    chan <- newChan 
    forkIO $ do
        ms <- getChanContents chan
        forM_ ms $ \m -> m >> done ()
    
    return (eDone, writeChan chan)

verifyStartImageFile :: Settings -> IO()
verifyStartImageFile settings = do
    let filePath = localPath startImageName
    startExist <- doesFileExist filePath
    when (not startExist) $ do
        putStrLn "creating starting image - one moment please..."
        renderView settings startView filePath
        putStrLn "done"    
