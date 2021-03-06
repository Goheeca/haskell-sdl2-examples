{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main (main) where

import Control.Monad.State hiding (state)
import Foreign.C.Types
import Foreign.Ptr
import GHC.Word
import SDL.Raw.Types
import Shared.Drawing
import Shared.Geometry
import Shared.Lifecycle
import Shared.Polling
import Shared.Textures
import Shared.Utilities
import Shared.State
import Shared.Image
import qualified SDL.Raw as SDL
import qualified SDL.Raw.Image as Image


title :: String
title = "lesson19"

size :: ScreenSize
size = (640, 480)

inWindow :: (SDL.Window -> IO ()) -> IO ()
inWindow = withSDL . withWindow title size

fullWindow :: SDL.Rect
fullWindow = SDL.Rect {
    rectX = 0,
    rectY = 0,
    rectW = fst size,
    rectH = snd size }

main :: IO ()
main = inWindow $ \window -> Shared.Image.withImgInit [Image.IMG_INIT_PNG] $ do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED, SDL.SDL_RENDERER_PRESENTVSYNC] >>= either throwSDLError return
    texture <- loadTexture renderer "./assets/arrow.png"
    gameController <- SDL.gameControllerOpen 0
    if gameController == nullPtr then fail "no controller found" else print "yay!"
    disableEventPolling [SDL.SDL_CONTROLLERAXISMOTION, SDL.SDL_JOYAXISMOTION]
    let initialWorld = World { gameover = False, getController = gameController, target = (320, 240) }
    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawWorld renderer [texture]
    _ <- runStateT (repeatUntilComplete pollDraw) initialWorld
    SDL.gameControllerClose gameController
    SDL.destroyTexture texture
    SDL.destroyRenderer renderer


data World = World { gameover :: Bool, getController :: SDL.GameController, target :: (CInt, CInt) }


disableEventPolling :: [Word32] -> IO ()
disableEventPolling = mapM_ (`SDL.eventState` 0)


drawWorld :: SDL.Renderer -> [SDL.Texture] -> World -> IO ()
drawWorld renderer assets world = withBlankScreen renderer $ do
    inputState <- getControllerState (getController world)
    with2 mask (position inputState) $ \mask' position' ->
        SDL.renderCopyEx renderer texture mask' position' degrees' nullPtr SDL.SDL_FLIP_NONE
    where texture = head assets
          w = 89 :: Integer
          h = 50 :: Integer
          sprite = toRect 0 0 w h
          mask = sprite
          position grrr = sprite `centredOn` fullWindow `moveBy` superScale grrr
          degrees' = 0

superScale :: (Double, Double) -> (Int, Int)
superScale = pairMap $ floor . (*) 200

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

getControllerState :: SDL.GameController -> IO (Double, Double)
getControllerState controller = do
    xValue <- getAxisState controller 0
    yValue <- getAxisState controller 1
    let range = hypotenuse xValue yValue
    let deadZone = 8000 ^ (2 :: Integer)
    let carpetValue = if range < deadZone
        then (0, 0)
        else pairMap ssscale (xValue, yValue)
    return carpetValue
      where ssscale x = fromIntegral x / 32768

hypotenuse :: (Num a) => a -> a -> a
hypotenuse a b = a ^ two + b ^ two
  where two = 2 :: Integer

getAxisState :: SDL.GameController -> SDL.GameControllerAxis -> IO Int
getAxisState controller index = do
    axis <- SDL.gameControllerGetAxis controller index
    return $ fromIntegral axis

updateState :: (Foldable f) => f SDL.Event -> World -> World
updateState events world = foldl applyEvent world events

applyEvent :: World -> SDL.Event -> World
applyEvent world (SDL.QuitEvent _ _) = world { gameover = True }
applyEvent world _ = world

repeatUntilComplete :: (Monad m) => m World -> m ()
repeatUntilComplete game = game >>= \world -> unless (gameover world) (repeatUntilComplete game)

