module Shared.Image where


import qualified SDL.Raw as SDL
import qualified SDL.Raw.Image as Image
import Shared.Lifecycle

import Data.Bits
import Data.Maybe
import Control.Monad
import Control.Exception
import GHC.Word
import Foreign.Ptr
import Foreign.C.String

failWithError :: String -> IO a
failWithError msg
    = do err <- fmap (fromMaybe "No SDL error") getError
         ioError $ userError $ msg ++ "\nSDL message: " ++ err

getSurfaceFrom' :: FilePath -> IO (Ptr SDL.Surface)
getSurfaceFrom' path = imgLoadSurface path >>= either throwSDLError return

imgLoadSurface :: String -> IO (Either String (Ptr SDL.Surface))
imgLoadSurface file
    = withCString file $ \cFile -> do
        tex <- _imgLoad cFile
        if tex == nullPtr
            then return (Left "IMG_LoadSurface(): Unknown error!")
            else return (Right tex)
foreign import ccall unsafe "IMG_Load" _imgLoad :: CString -> IO (Ptr SDL.Surface)

toBitmask :: (Bits b,Num b) => (a -> b) -> [a] -> b
toBitmask from = foldr (.|.) 0 . map from

withImgInit :: [Image.InitFlags] -> IO a -> IO a
withImgInit flags action
    = bracket_ (imgInit flags) imgQuit action

imgQuit :: IO ()
imgQuit = _imgQuit
foreign import ccall unsafe "IMG_Quit" _imgQuit :: IO ()

imgInit :: [Image.InitFlags] -> IO ()
imgInit flags
    = do ret <- _imgInit (fromIntegral (toBitmask id flags))
         when (ret == (-1)) (failWithError "SDL_Init")
foreign import ccall unsafe "IMG_Init" _imgInit :: Word32 -> IO Int

imgLoadTexture :: SDL.Renderer -> String -> IO (Either String SDL.Texture)
imgLoadTexture rend file
    = withCString file $ \cFile -> do
        tex <- _imgLoadTexture rend cFile
        err <- getError
        if tex == nullPtr
            then return (Left (fromMaybe "IMG_LoadTexture(): Unknown error!" err))
            else return (Right tex)
foreign import ccall unsafe "IMG_LoadTexture" _imgLoadTexture :: SDL.Renderer -> CString -> IO SDL.Texture

getError :: IO (Maybe String)
getError
    = do str <- peekCString =<< _sdlGetError
         if null str
            then return Nothing
            else return (Just str)
foreign import ccall unsafe "SDL_GetError" _sdlGetError :: IO CString