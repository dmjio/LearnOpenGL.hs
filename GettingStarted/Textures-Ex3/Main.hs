{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}
module Main where

import           Codec.Picture
import           Codec.Picture.Extra    (flipVertically)
import           Codec.Picture.Types
import qualified Codec.Picture.Types    as CPTI
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.ByteString.Unsafe
import           Data.List              (intercalate, isSuffixOf, isPrefixOf)
import           Data.Vector.Storable   (Vector)
import qualified Data.Vector.Storable   as V
import           Data.Word
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL
import           Graphics.UI.GLFW
import           Prelude                hiding (init)
import           System.Exit

main :: IO ()
main = do
  init
  windowHint (WindowHint'ContextVersionMajor 3)
  windowHint (WindowHint'ContextVersionMinor 3)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  windowHint (WindowHint'OpenGLForwardCompat True)
  windowHint (WindowHint'OpenGLDebugContext True)
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
      shaderProgram <- makeShaderProgram
      initBuffers $ \vaoPtr vboPtr eboPtr containerPtr awesomeFacePtr -> do
        glUseProgram shaderProgram
        loc <- glGetUniformLocation shaderProgram =<< newCString "texture1"
        glUniform1i loc 0
        loc <- glGetUniformLocation shaderProgram =<< newCString "texture2"
        glUniform1i loc 1
        forever $ do
          shouldClose <- windowShouldClose window
          if shouldClose
            then do
              glDeleteProgram shaderProgram
              glDeleteVertexArrays 1 vaoPtr
              glDeleteBuffers 1 vboPtr
              glDeleteBuffers 1 eboPtr
              terminate
              exitSuccess
            else do
              process window
              render shaderProgram vaoPtr containerPtr awesomeFacePtr window
              swapBuffers window
              pollEvents

dumpError :: IO ()
dumpError = print =<< glGetError

proxySizeOf :: forall a p. Storable a => p a -> Int
proxySizeOf _ = sizeOf (undefined :: a)

printGLVersion :: IO ()
printGLVersion =
  putStrLn =<<
    peekCString =<<
      castPtr <$>
        glGetString GL_VERSION

makeShaderProgram :: IO GLuint
makeShaderProgram =
  join $ createShaderProgram
    <$> compileVertexShader
    <*> compileFragmentShader

type VAO = Ptr GLuint
type VBO = Ptr GLuint
type EBO = Ptr GLuint
type TEXTURE = Ptr GLuint

floatSize :: Int
floatSize = sizeOf (undefined :: Float)

initBuffers :: (VBO -> VAO -> EBO -> TEXTURE -> TEXTURE -> IO ()) -> IO ()
initBuffers callback = do
 alloca $ \eboPtr -> do
  alloca $ \vaoPtr -> do
   alloca $ \vboPtr -> do
    glGenVertexArrays 1 vaoPtr
    glGenBuffers 1 vboPtr
    glGenBuffers 1 eboPtr
    peek vaoPtr >>= glBindVertexArray
    peek vboPtr >>= glBindBuffer GL_ARRAY_BUFFER
    V.unsafeWith verts $ \vertsPtr ->
      glBufferData
        GL_ARRAY_BUFFER
        (fromIntegral (V.length verts * proxySizeOf verts))
        (castPtr vertsPtr)
        GL_STATIC_DRAW
    peek eboPtr >>= glBindBuffer GL_ELEMENT_ARRAY_BUFFER
    V.unsafeWith indices $ \indicesPtr ->
      glBufferData
        GL_ELEMENT_ARRAY_BUFFER
        (fromIntegral (V.length indices * proxySizeOf indices))
        (castPtr indicesPtr)
        GL_STATIC_DRAW

    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
      (fromIntegral (floatSize * 8))
        nullPtr

    glEnableVertexAttribArray 0

    glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE
      (fromIntegral (floatSize * 8))
        (nullPtr `plusPtr` (3 * floatSize))

    glEnableVertexAttribArray 1

    glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE
      (fromIntegral (floatSize * 8))
        (nullPtr `plusPtr` (6 * floatSize))

    glEnableVertexAttribArray 2

    -- glEnableVertexAttribArray 0
    -- glBindBuffer GL_ARRAY_BUFFER 0
    -- glBindVertexArray 0

    loadTexture "img/container.jpg" GL_CLAMP_TO_EDGE $ \container ->
      loadTexture "img/awesomeface.png" GL_REPEAT $ \face ->
        callback vaoPtr vboPtr eboPtr container face

toImage
  :: String
  -> ByteString
  -> (Ptr Word8 -> GLint -> GLint -> IO ())
  -> IO ()
toImage name bytes go =
  case decodeImage bytes of
    Right (ImageYCbCr8 i) ->
      V.unsafeWith (imageData (convertImage i :: CPTI.Image PixelRGB8)) $ \ptr ->
        go ptr (fromIntegral (imageWidth i)) (fromIntegral (imageHeight i))
    Right (ImageRGBA8 i) ->
      V.unsafeWith (imageData (flipVertically (convertImage i :: CPTI.Image PixelRGBA8))) $ \ptr ->
        go ptr (fromIntegral (imageWidth i)) (fromIntegral (imageHeight i))
    _  -> do
      putStrLn name
      exitFailure

loadTexture :: String -> GLint -> (TEXTURE -> IO ()) -> IO ()
loadTexture name style f = do
  alloca $ \texturePtr -> do
    glGenTextures 1 texturePtr
    glBindTexture GL_TEXTURE_2D =<< peek texturePtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S style
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T style
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    bytes <- B.readFile name
    let typ | ".png" `isSuffixOf` name = GL_RGBA | otherwise = GL_RGB
    toImage name bytes $ \ptr w h -> do
      glTexImage2D GL_TEXTURE_2D 0
        (fromIntegral typ) w h
          0 typ GL_UNSIGNED_BYTE
            (castPtr ptr)
      glGenerateMipmap GL_TEXTURE_2D
      f texturePtr

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

compileFragmentShader :: IO GLuint
compileFragmentShader = do
  putStrLn "Compiling fragment shader"
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  fragmentShaderSource <- newCString =<< getFragmentShader
  alloca $ \shadersStr -> do
    shadersStr `poke` fragmentShaderSource
    glShaderSource fragmentShader 1 shadersStr nullPtr
    glCompileShader fragmentShader
    checkShaderCompilation fragmentShader
  pure fragmentShader

getFragmentShader :: IO String
getFragmentShader = readFile "frag.glsl"

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

verts :: Vector Float
verts =
  V.fromList
    -- positions    -- colors      -- text coords
  [  0.5,  0.5, 0.0, 1.0, 0.0, 0.0, 0.55, 0.55
  ,  0.5, -0.5, 0.0, 0.0, 1.0, 0.0, 0.55, 0.45
  , -0.5, -0.5, 0.0, 0.0, 0.0, 1.0, 0.45, 0.45
  , -0.5,  0.5, 0.0, 1.0, 1.0, 0.0, 0.45, 0.55
  ]

indices :: Vector Word32
indices = V.fromList
  [ 0,1,3
  , 1,2,3
  ]

render
  :: GLuint
  -> VAO
  -> TEXTURE
  -> TEXTURE
  -> Window
  -> IO ()
render shaderProgram vaoPtr containerPtr awesomePtr window = do
  vao <- peek vaoPtr
  -- glPolygonMode GL_FRONT_AND_BACK GL_LINE
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT

  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D =<< peek containerPtr

  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D =<< peek awesomePtr

  glUseProgram shaderProgram
  glBindVertexArray vao
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr

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
