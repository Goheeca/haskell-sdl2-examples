module Main (main) where

import qualified SDL.Raw as SDL
import qualified SDL.Raw.Image as Image
import Shared.Image
import Shared.Input
import Shared.Lifecycle
import Shared.Polling
import Shared.Utilities
import Shared.Textures


title :: String
title = "lesson07"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

main :: IO ()
main = inWindow $ \window -> Shared.Image.withImgInit [Image.IMG_INIT_PNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    texture <- loadTexture renderer "./assets/texture.png"
    repeatUntilTrue $ draw renderer texture >> pollForQuit pollEvent
    SDL.destroyTexture texture
    SDL.destroyRenderer renderer

draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw renderer texture = do
    _ <- SDL.renderClear renderer
    _ <- SDL.renderCopy renderer texture nullPtr nullPtr
    SDL.renderPresent renderer

