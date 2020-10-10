{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP  #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.List              (intercalate)
import           Data.Vector.Storable   (Vector)
import qualified Data.Vector.Storable   as V
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL
import           Graphics.UI.GLFW
import           Prelude                hiding (init)
import           System.Exit

-- iforM_ :: (Monad m, Storable t) => [Vector t] -> (Int -> Vector t -> m b) -> m ()
-- iforM_ xs f = do
--   forM_ (zip [ 0 .. length xs - 1 ] xs) $ \(idx, vec) ->
--     f idx vec

main :: IO ()
main = do
  init
  windowHint (WindowHint'ContextVersionMajor 3)
  windowHint (WindowHint'ContextVersionMinor 3)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  windowHint (WindowHint'OpenGLForwardCompat True)
  maybeWindow <- createWindow 800 600 "Learn OpenGL" Nothing Nothing
  case maybeWindow of
    Nothing -> do
      putStrLn "Failed to create GLFW window"
      terminate
      exitFailure
    Just window -> do
      makeContextCurrent (Just window)
      printGLVersion
      setFramebufferSizeCallback window (Just frameBufferSizeCallback)
      shaderProgram1 <- makeShaderProgram "frag.glsl"
      shaderProgram2 <- makeShaderProgram "frag2.glsl"
      initBuffers [ triangle1, triangle2 ] $ \vaoPtr ->
        forever $ do
          shouldClose <- windowShouldClose window
          if shouldClose
            then do
              glDeleteProgram shaderProgram1
              glDeleteProgram shaderProgram2
              terminate
              exitSuccess
            else do
              let shaderPrograms = [ shaderProgram1, shaderProgram2 ]
              process window
              render shaderPrograms vaoPtr window
              swapBuffers window
              pollEvents

proxySizeOf :: forall a p. Storable a => p a -> Int
proxySizeOf _ = sizeOf (undefined :: a)

printGLVersion :: IO ()
printGLVersion =
  putStrLn =<<
    peekCString =<<
      castPtr <$>
        glGetString GL_VERSION

makeShaderProgram :: String -> IO GLuint
makeShaderProgram name =
  join $ createShaderProgram
    <$> compileVertexShader
    <*> compileFragmentShader name

type VAO = Ptr GLuint
type VBO = Ptr GLuint

floatSize = sizeOf (undefined :: Float)

initBuffers
  :: forall a . Storable a
  => [Vector a]
  -> (VAO -> IO ())
  -> IO ()
initBuffers [vertsA, vertsB] f = do
  let len = fromIntegral 2
  alloca $ \vaoPtr -> do
    alloca $ \vboPtr -> do
      glGenVertexArrays len vaoPtr
      glGenBuffers len vboPtr

      peek (vaoPtr `plusPtr` 0) >>= glBindVertexArray
      peek (vboPtr `plusPtr` 0) >>= glBindBuffer GL_ARRAY_BUFFER

      V.unsafeWith vertsA $ \vertsPtr ->
        glBufferData
          GL_ARRAY_BUFFER
          (fromIntegral (V.length vertsA * proxySizeOf vertsA))
          (castPtr vertsPtr)
          GL_STATIC_DRAW
      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
        (fromIntegral (floatSize * 3))
          nullPtr
      glEnableVertexAttribArray 0

      peek (vaoPtr `plusPtr` 4) >>= glBindVertexArray
      peek (vboPtr `plusPtr` 4) >>= glBindBuffer GL_ARRAY_BUFFER
      V.unsafeWith vertsB $ \vertsPtr ->
        glBufferData
          GL_ARRAY_BUFFER
          (fromIntegral (V.length vertsB * proxySizeOf vertsB))
          (castPtr vertsPtr)
          GL_STATIC_DRAW
      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
      glEnableVertexAttribArray 0

      f vaoPtr

createShaderProgram
  :: GLuint
  -> GLuint
  -> IO GLuint
createShaderProgram vertexShader fragmentShader = do
  shaderProgram <- glCreateProgram
  glAttachShader shaderProgram vertexShader
  glAttachShader shaderProgram fragmentShader
  glLinkProgram shaderProgram
  checkShaderLinking shaderProgram
  glDeleteShader vertexShader
  glDeleteShader fragmentShader
  pure shaderProgram

checkShaderLinking :: GLuint -> IO ()
checkShaderLinking shaderProgram = do
  alloca $ \successPtr -> do
    alloca $ \infoLogPtr -> do
      glGetProgramiv shaderProgram GL_LINK_STATUS successPtr
      success <- peek successPtr
      if success <= 0
        then do
          glGetProgramInfoLog shaderProgram 512 nullPtr infoLogPtr
          putStrLn =<< peekCString infoLogPtr
          exitFailure
        else do
          putStrLn "Successfully linked shader program"

compileVertexShader :: IO GLuint
compileVertexShader = do
  putStrLn "Compiling vertex shader"
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  vertexShaderSource <- newCString =<< getVertexShader
  alloca $ \shadersStr -> do
    shadersStr `poke` vertexShaderSource
    glShaderSource vertexShader 1 shadersStr nullPtr
    glCompileShader vertexShader
    checkShaderCompilation vertexShader
  pure vertexShader

getVertexShader :: IO String
getVertexShader = readFile "vert.glsl"

compileFragmentShader :: String -> IO GLuint
compileFragmentShader name = do
  putStrLn "Compiling fragment shader"
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  fragmentShaderSource <- newCString =<< getFragmentShader name
  alloca $ \shadersStr -> do
    shadersStr `poke` fragmentShaderSource
    glShaderSource fragmentShader 1 shadersStr nullPtr
    glCompileShader fragmentShader
    checkShaderCompilation fragmentShader
  pure fragmentShader

getFragmentShader :: String -> IO String
getFragmentShader = readFile

checkShaderCompilation :: GLuint -> IO ()
checkShaderCompilation shader = do
  alloca $ \successPtr ->
    alloca $ \infoLogPtr -> do
      glGetShaderiv shader GL_COMPILE_STATUS successPtr
      success <- peek successPtr
      glGetShaderInfoLog shader 512 nullPtr infoLogPtr
      mapM_ print =<< lines <$> peekCString infoLogPtr
      when (success <= 0) $ do
        putStrLn "Failed to compile shader "
        exitFailure
      putStrLn "Compiled shader successfully"

triangle1 :: Vector Float
triangle1
  = V.fromList
  [ -0.9, -0.5, 0.0,
    -0.0, -0.5, 0.0,
    -0.45, 0.5, 0.0
   ]

triangle2 :: Vector Float
triangle2 = V.fromList [
    0.0, -0.5, 0.0,
    0.9, -0.5, 0.0,
    0.45, 0.5, 0.0
  ]

render
  :: [GLuint]
  -> Ptr GLuint
  -> Window
  -> IO ()
render [shaderProgram1, shaderProgram2] vaoPtr window = do
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT
  glUseProgram shaderProgram1

  glBindVertexArray =<< peek (vaoPtr `plusPtr` 0)
  glDrawArrays GL_TRIANGLES 0 3

  glUseProgram shaderProgram2

  glBindVertexArray =<< peek (vaoPtr `plusPtr` 4)
  glDrawArrays GL_TRIANGLES 0 3

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
