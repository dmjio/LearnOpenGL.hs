module Main where

import Prelude hiding (init)
import Control.Monad
import Foreign.Ptr
import Graphics.GL
import Graphics.UI.GLFW
import System.Exit

main :: IO ()
main = void init >> do
  maybeWindow <-
    createWindow 800 600 "Learn OpenGL" Nothing Nothing
  case maybeWindow of
    Nothing -> do
      putStrLn "Failed to create GLFW window"
      terminate
      exitFailure
    Just window -> do
      makeContextCurrent (Just window)
      setFramebufferSizeCallback window (Just frameBufferSizeCallback)
      forever $ do
        shouldClose <- windowShouldClose window
        if shouldClose
          then do
            terminate
            exitSuccess
          else do
            process window
            render window
            swapBuffers window
            pollEvents

render
  :: Window
  -> IO ()
render window = do
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT

process
  :: Window
  -> IO ()
process window = do
  keyState <- getKey window Key'Escape
  case keyState of
    KeyState'Pressed -> setWindowShouldClose window True
    _ -> pure ()

frameBufferSizeCallback
  :: Window
  -> Int
  -> Int
  -> IO ()
frameBufferSizeCallback _ x y =
  glViewport 0 0
    (fromIntegral x)
    (fromIntegral y)
