module Shared.Lifecycle (
  withSDL,
  withWindow,
  throwSDLError
) where

import qualified Graphics.UI.SDL as SDL
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (nullPtr)
import GHC.Word

type Risky a = Either String a

initializeSDL :: [Word32] -> IO (Risky ())
initializeSDL flags = do
    initSuccess <- SDL.init $ foldl (.|.) 0 flags
    return $ if initSuccess < 0 then Left "SDL could not initialize!" else Right ()

withSDL :: IO () -> IO ()
withSDL op = do
    initializeSDL [SDL.SDL_INIT_VIDEO] >>= either throwSDLError return
    op
    SDL.quit

withWindow :: String -> (CInt, CInt) -> (SDL.Window -> IO ()) -> IO ()
withWindow title size op = do
    window <- createWindow title size >>= either throwSDLError return
    op window
    SDL.destroyWindow window

createWindow :: String -> (CInt, CInt) -> IO (Risky SDL.Window)
createWindow title (w, h) = withCAString title $ \ctitle -> do
    window <- SDL.createWindow ctitle SDL.SDL_WINDOWPOS_UNDEFINED SDL.SDL_WINDOWPOS_UNDEFINED w h SDL.SDL_WINDOW_SHOWN
    return $ if window == nullPtr then Left "Window could not be created!" else Right window


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)
