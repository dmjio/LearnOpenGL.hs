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

import Data.Function ((&))
import           Codec.Picture
import           Codec.Picture.Extra    (flipVertically)
import           Codec.Picture.Types
import qualified Codec.Picture.Types    as CPTI
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.ByteString.Unsafe
import qualified Data.Foldable          as F
import           Data.IORef
import           Data.List              (intercalate, isSuffixOf, isPrefixOf)
import qualified Data.List              as DL
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
import           System.IO

import           Linear

-- Vector length, euclidean distance
pythagoras :: (Floating c, Foldable t, Functor t) => t c -> c
pythagoras = sqrt . sum . fmap (^2)

len :: (Floating c, Foldable t, Functor t) => t c -> c
len = pythagoras

unitVector :: (Functor t, Floating c, Foldable t) => t c -> t c
unitVector v = v ^/ pythagoras v

-- | Dot product over a vector space
-- λ> dotp [V2 1.2 3.4, V2 5.6 7.8]
-- 33.24
dotp :: (Foldable t, Num a, Num (t a)) => [t a] -> a
dotp = sum . product

-- | Dot product
-- λ> dotp' [1.2,3.4] [5.6,7.8]
-- 33.24
dotp' :: Num a => [a] -> [a] -> a
dotp' xs ys = sum (zipWith (*) xs ys)

crossP :: Num a => V3 a -> V3 a -> V3 a
crossP = cross
  where
    foo = [4 + read "3"]

distance (x1,y1) (x2,y2) = sqrt (x + y)
  where
    y = (y2 - y1) ^ 2
    x = (x2 - x1) ^ 2

degreesToRadians 0 = 0
degreesToRadians x = pi / (180 / x)

height, width :: Int
height = 600
width  = 800

lastX, lastY :: Float
lastX = fromIntegral width / 2
lastY = fromIntegral height / 2

main :: IO ()
main = do
  init
  -- dumpError "init"
  windowHint (WindowHint'ContextVersionMajor 3)
  windowHint (WindowHint'ContextVersionMinor 3)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  windowHint (WindowHint'OpenGLForwardCompat True)
  windowHint (WindowHint'OpenGLDebugContext True)
  maybeWindow <- createWindow width height "Learn OpenGL" Nothing Nothing
  cameraRef <- newIORef defaultCamera { position = V3 0 0 3 }
  firstMouseRef <- newIORef True
  lastXRef <- newIORef lastX
  lastYRef <- newIORef lastY
  case maybeWindow of
    Nothing -> do
      putStrLn "Failed to create GLFW window"
      terminate
      exitFailure
    Just window -> do
      makeContextCurrent (Just window)
      printGLVersion
      dumpError "gl version"
      setFramebufferSizeCallback window (Just frameBufferSizeCallback)
      setScrollCallback window (Just (scrollCallback cameraRef))
      setCursorPosCallback window (Just (mousePosCallback cameraRef firstMouseRef lastXRef lastYRef))
      setCursorInputMode window CursorInputMode'Disabled
      glEnable GL_DEPTH_TEST
      dumpError "glEnable"
      shaderProgram <- makeShaderProgram "vert.glsl" "frag.glsl"
      dumpError "shader"
      initBuffers $ \vaoPtr vboPtr containerPtr awesomeFacePtr -> do
        glUseProgram shaderProgram
        dumpError "use program"
        setInt shaderProgram "texture1" 0
        dumpError "set texture1"
        setInt shaderProgram "texture2" 1
        dumpError "set texture2"
        hFlush stdout
        ref <- newIORef (0.2 :: Float)
        lastFrameRef <- newIORef 0.0
        deltaRef <- newIORef 0.0
        forever $ do
          shouldClose <- windowShouldClose window
          if shouldClose
            then do
              glDeleteProgram shaderProgram
              glDeleteVertexArrays 1 vaoPtr
              glDeleteBuffers 1 vboPtr
              terminate
              exitSuccess
            else do
              process window ref cameraRef deltaRef
              render shaderProgram vaoPtr containerPtr awesomeFacePtr window ref
                cameraRef lastFrameRef deltaRef
              swapBuffers window
              pollEvents

debug :: Bool
debug = True

dumpError :: String -> IO ()
dumpError str = do
  when debug $ do
    code <- glGetError
    unless (code == 0) $ do
      print code
      putStrLn str
      exitFailure

proxySizeOf :: forall a p. Storable a => p a -> Int
proxySizeOf _ = sizeOf (undefined :: a)

printGLVersion :: IO ()
printGLVersion =
  putStrLn =<<
    peekCString =<<
      castPtr <$>
        glGetString GL_VERSION

makeShaderProgram :: String -> String -> IO GLuint
makeShaderProgram vertShaderFile fragShaderFile = do
  v <- compileShader GL_VERTEX_SHADER vertShaderFile
  dumpError "vert shader"
  f <- compileShader GL_FRAGMENT_SHADER fragShaderFile
  dumpError "frag shader"
  createShaderProgram v f

type VAO = Ptr GLuint
type VBO = Ptr GLuint
type EBO = Ptr GLuint
type TEXTURE = Ptr GLuint

floatSize :: Int
floatSize = sizeOf (undefined :: Float)

initBuffers :: (VBO -> VAO -> TEXTURE -> TEXTURE -> IO ()) -> IO ()
initBuffers callback = do
  alloca $ \vaoPtr -> do
   alloca $ \vboPtr -> do
    glGenVertexArrays 1 vaoPtr
    dumpError "glGenVertexArrays 1 vaoPtr"
    glGenBuffers 1 vboPtr
    dumpError "glGenBuffers 1 vboPtr"
    peek vaoPtr >>= glBindVertexArray
    peek vboPtr >>= glBindBuffer GL_ARRAY_BUFFER
    V.unsafeWith verts $ \vertsPtr ->
      glBufferData
        GL_ARRAY_BUFFER
        (fromIntegral (V.length verts * proxySizeOf verts))
        (castPtr vertsPtr)
        GL_STATIC_DRAW
    dumpError "glBufferData 1"

    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
      (fromIntegral (floatSize * 5))
        nullPtr
    dumpError "glVertexAttribPointer 0 3"

    glEnableVertexAttribArray 0
    dumpError "glEnableVertexAttribArray 0"

    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE
      (fromIntegral (floatSize * 5))
        (nullPtr `plusPtr` (3 * floatSize))
    dumpError "glVertexAttribPointer 0"

    glEnableVertexAttribArray 1
    dumpError "glEnableVertexAttribArray 1"

    loadTexture "img/container.jpg" GL_REPEAT $ \container ->
      loadTexture "img/awesomeface.png" GL_REPEAT $ \face ->
        callback vaoPtr vboPtr container face

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
  putStrLn ("loading texture: " <> name)
  alloca $ \texturePtr -> do
    glGenTextures 1 texturePtr
    dumpError "glGenTextures 1 texturePtr"
    glBindTexture GL_TEXTURE_2D =<< peek texturePtr
    dumpError "glBindTexture GL_TEXTURE_2D =<< peek texturePtr"
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S style
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T style
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    bytes <- B.readFile name
    let typ | ".png" `isSuffixOf` name = GL_RGBA | otherwise = GL_RGB
    toImage name bytes $ \ptr w h -> do
      glTexImage2D GL_TEXTURE_2D 0
        GL_RGB w h
          0 typ GL_UNSIGNED_BYTE
            (castPtr ptr)
      dumpError "glTexImage2D"
      glGenerateMipmap GL_TEXTURE_2D
      dumpError "glGenMipmap"
      f texturePtr

withPtr
  :: (Functor f, Storable a, Floating a, Foldable f, Foldable g)
  => f (g a)
  -> (Ptr a -> IO ())
  -> IO ()
withPtr
  = V.unsafeWith
  . V.fromList
  . concat
  . DL.transpose
  . F.toList
  . fmap F.toList

translated :: V3 Float -> M44 Float
translated = mkTransformation (axisAngle (V3 0 0 0) 0)

rotated :: Float -> V3 Float -> M44 Float
rotated degrees vec = mkTransformation (axisAngle vec degrees) (V3 0 0 0)

createShaderProgram
  :: GLuint
  -> GLuint
  -> IO GLuint
createShaderProgram vertexShader fragmentShader = do
  shaderProgram <- glCreateProgram
  dumpError "create program"
  glAttachShader shaderProgram vertexShader
  dumpError "attach1"
  glAttachShader shaderProgram fragmentShader
  dumpError "attach2"
  glLinkProgram shaderProgram
  dumpError "linking"
  checkShaderLinking shaderProgram
  dumpError "check linking"
  glDeleteShader vertexShader
  dumpError "delete vert shader"
  glDeleteShader fragmentShader
  dumpError "delete frag shader"
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

compileShader :: GLuint -> String -> IO GLuint
compileShader shaderType shaderFile = do
  putStrLn $ "Compiling " <> shaderFile
  shader <- glCreateShader shaderType
  shaderSource' <- readFile shaderFile
  putStrLn shaderSource'
  withCString shaderSource' $ \shaderSource ->
    alloca $ \shadersStr -> do
      shadersStr `poke` shaderSource
      glShaderSource shader 1 shadersStr nullPtr
      glCompileShader shader
      checkShaderCompilation shader
  pure shader

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
verts
  = V.fromList
  [ -0.5, -0.5, -0.5,  0.0, 0.0,
     0.5, -0.5, -0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5,  0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 0.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
    -0.5,  0.5,  0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5,  0.5,  0.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  1.0, 1.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5,  0.5, -0.5,  0.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5, -0.5,  0.0, 1.0
  ]

render
  :: GLuint
  -> VAO
  -> TEXTURE
  -> TEXTURE
  -> Window
  -> IORef Float
  -> IORef Camera
  -> IORef Float
  -> IORef Float
  -> IO ()
render shaderProgram vaoPtr containerPtr awesomePtr window ref cameraRef lastFrameRef deltaRef = do

  -- glPolygonMode GL_FRONT_AND_BACK GL_LINE
  glClearColor 0.2 0.3 0.3 1.0
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  glActiveTexture GL_TEXTURE0
  dumpError "glActiveTexture GL_TEXTURE0"

  glBindTexture GL_TEXTURE_2D =<< peek containerPtr
  dumpError "glDrawArrays GL_TRIANGLES 0 36"

  glActiveTexture GL_TEXTURE1
  dumpError "glActiveTexture GL_TEXTURE1"

  glBindTexture GL_TEXTURE_2D =<< peek awesomePtr
  dumpError "glBindTexture GL_TEXTURE_2D =<< peek containerPtr"

  glUseProgram shaderProgram
  dumpError "glUseProgram shaderProgram"

  setFloat shaderProgram "mixValue" =<< readIORef ref

  let radius = 10.0 :: Float
  Just (realToFrac -> time) <- getTime

  lastFrame <- readIORef lastFrameRef
  writeIORef deltaRef $! time - lastFrame
  writeIORef lastFrameRef $! time

  view' <- getViewMatrix <$> readIORef cameraRef
  zoom' <- zoom <$> readIORef cameraRef

  let
    view :: M44 Float
    view = view'

    projection :: M44 Float
    projection =
      perspective
        (degreesToRadians zoom')
        (fromIntegral width / fromIntegral height)
        0.1
        100.0

  setMatrix False shaderProgram "view" view
  setMatrix False shaderProgram "projection" projection

  peek vaoPtr >>= glBindVertexArray

  forM_ (zip [1..] positions) $ \(i :: Int,position) -> do
    Just (realToFrac -> time) <- getTime
    setMatrix False shaderProgram "model" $
      translated position !*!
        rotated (if i `mod` 3 == 0 || i == 1
                 then time * fromIntegral i / 2
                 else fromIntegral $ i * 20)
          (V3 1 0.3 0.5)
    glDrawArrays GL_TRIANGLES 0 36

  dumpError "glDrawArrays GL_TRIANGLES 0 36"

positions :: [V3 Float]
positions =
  [ V3  0.0  0.0  0.0
  , V3  2.0  5.0 (-15.0)
  , V3 (-1.5) (-2.2) (-2.5)
  , V3 (-3.8) (-2.0) (-12.3)
  , V3  2.4 (-0.4) (-3.5)
  , V3 (-1.7)  3.0 (-7.5)
  , V3  1.3 (-2.0) (-2.5)
  , V3  1.5  2.0 (-2.5)
  , V3  1.5  0.2 (-1.5)
  , V3 (-1.3)  1.0 (-1.5)
  ]

setFloat :: GLuint -> String -> GLfloat -> IO ()
setFloat id' name val = do
  withCString name $ \cstr -> do
    location <- glGetUniformLocation id' cstr
    glUniform1f location val

setInt :: GLuint -> String -> GLint -> IO ()
setInt id' name val = do
  withCString name $ \cstr -> do
    location <- glGetUniformLocation id' cstr
    glUniform1i location val

setMatrix :: Bool -> GLuint -> String -> M44 Float -> IO ()
setMatrix flipped id' name val = do
  withCString name $ \cstr -> do
    location <- glGetUniformLocation id' cstr
    withPtr val
      $ glUniformMatrix4fv location 1
      $ fromIntegral (popCount flipped)

process
  :: Window
  -> IORef Float
  -> IORef Camera
  -> IORef Float
  -> IO ()
process window ref cameraRef deltaRef = do
  delta <- readIORef deltaRef
  keyState <- getKey window Key'Escape
  case keyState of
    KeyState'Pressed ->
      setWindowShouldClose window True
    _ -> pure ()

  keyState <- getKey window Key'Up
  case keyState of
    KeyState'Pressed -> do
      modifyIORef' ref $ \x -> do
        case x + 0.01 of
          newX | newX >= 1.0 -> 1.0
               | otherwise -> newX
    _ -> pure ()

  keyState <- getKey window Key'Down
  case keyState of
    KeyState'Pressed -> do
      modifyIORef' ref $ \x -> do
        case x - 0.01 of
          newX | newX <= 0.0 -> 0.0
               | otherwise -> newX
    _ -> pure ()

  keyState <- getKey window Key'W
  case keyState of
    KeyState'Pressed -> do
      modifyIORef' cameraRef $ \camera -> do
        processKeyboard CameraForward delta camera
    _ -> pure ()

  keyState <- getKey window Key'A
  case keyState of
    KeyState'Pressed -> do
      modifyIORef' cameraRef $ \camera -> do
        processKeyboard CameraLeft delta camera
    _ -> pure ()

  keyState <- getKey window Key'S
  case keyState of
    KeyState'Pressed -> do
      modifyIORef' cameraRef $ \camera -> do
        processKeyboard CameraBackward delta camera
    _ -> pure ()

  keyState <- getKey window Key'D
  case keyState of
    KeyState'Pressed -> do
      modifyIORef' cameraRef $ \camera -> do
        processKeyboard CameraRight delta camera
    _ -> pure ()

frameBufferSizeCallback
  :: Window
  -> Int
  -> Int
  -> IO ()
frameBufferSizeCallback _ width height =
  glViewport 0 0
    (fromIntegral width)
    (fromIntegral height)

mousePosCallback
  :: IORef Camera
  -> IORef Bool
  -> IORef Float
  -> IORef Float
  -> Window
  -> Double
  -> Double
  -> IO ()
mousePosCallback cameraRef mouseRef lastXRef lastYRef window xpos ypos = do
  firstMouse <- readIORef mouseRef

  when firstMouse $ do
    writeIORef lastXRef (realToFrac xpos)
    writeIORef lastYRef (realToFrac ypos)
    writeIORef mouseRef False

  lastX <- readIORef lastXRef
  lastY <- readIORef lastYRef

  let
    xOffset  = xpos - realToFrac lastX
    yOffset  = ypos - realToFrac lastY

  writeIORef lastXRef (realToFrac xpos)
  writeIORef lastYRef (realToFrac ypos)

  modifyIORef' cameraRef
    (processMouseMovement
       (realToFrac xOffset) (realToFrac yOffset)
          True)

scrollCallback
  :: IORef Camera
  -> Window
  -> Double
  -> Double
  -> IO ()
scrollCallback cameraRef _ _ yOffset =
  modifyIORef' cameraRef (processMouseScroll (realToFrac yOffset))

data Camera
  = Camera
  { position
  , front
  , up
  , right
  , worldUp :: V3 Float
  , yaw
  , pitch
  , speed
  , sensitivity
  , zoom :: Float
  } deriving (Show, Eq)

defaultCamera :: Camera
defaultCamera = do
  let
    worldUp     = V3 0 1 0
    front       = V3 0 0 (-1)
    position    = V3 0 0 0
    yaw         = (-90)
    pitch       = 0
    speed       = 2.5
    sensitivity = 0.1
    zoom        = 45
    right       = normalize (cross front worldUp)
  updateCameraVectors
    Camera
      { up = normalize (cross right front)
      , ..
      }

calcFront
  :: Float
  -- ^ yaw
  -> Float
  -- ^ pitch
  -> V3 Float
calcFront yaw pitch = do
  let
    x = cos (degreesToRadians yaw) * cos (degreesToRadians pitch)
    y = sin (degreesToRadians pitch)
    z = sin (degreesToRadians yaw) * cos (degreesToRadians pitch)
  normalize (V3 x y z)

updateCameraVectors
  :: Camera
  -> Camera
updateCameraVectors camera = do
  let
    newCamera =
      camera
      { front = calcFront (yaw camera) (pitch camera)
      }
  newCamera
    { right = normalize $ cross (front newCamera) (worldUp newCamera)
    , up = normalize $ cross (right newCamera) (front newCamera)
    }

processMouseScroll
  :: Float
  -> Camera
  -> Camera
processMouseScroll yoffset camera = do
  let updated = camera { zoom = zoom camera - yoffset }
      updatedCamera
        | zoom updated < 1  = updated { zoom = 1 }
        | zoom updated > 45 = updated { zoom = 45 }
        | otherwise         = updated
  updatedCamera

-- | (negate -> yOffset) inverts the Y axis.
processMouseMovement :: Float -> Float -> Bool -> Camera -> Camera
processMouseMovement xOffset (negate -> yOffset) constraint c = do
  let
    xOffset' = sensitivity c * xOffset
    yOffset' = sensitivity c * yOffset
    yaw'     = xOffset' + yaw c
    pitch'   = yOffset' + pitch c
    pitch''  =
      if constraint
      then do
        let pitch''
              | pitch' > 89    = 89
              | pitch' < (-89) = (-89)
              | otherwise      = pitch'
        pitch''
      else
        pitch'
  updateCameraVectors c
    { yaw = yaw'
    , pitch = pitch''
    }

getViewMatrix
  :: Camera
  -> M44 Float
getViewMatrix c
-- # TODO: fix me
-- = myLookAt (position c) (position c + front c) (up c)
  = lookAt (position c) (position c + front c) (up c)

-- broken ...
myLookAt :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a -> M44 a
myLookAt position target worldUp = do
  let
    zaxis = normalize (position - target)
    xaxis = normalize (cross (normalize worldUp) zaxis)
    yaxis = cross zaxis yaxis
    r = m33_to_m44 (V3 xaxis yaxis zaxis)
    t = do
      let setW x (V4 a b c _) = V4 a b c x
          V3 px py pz = negate position
          V4 x y z w  = identity
      V4 (setW px x) (setW py y) (setW pz z) (V4 0 0 0 1)
  r !*! t

processKeyboard
  :: CameraDirection
  -> Float
  -> Camera
  -> Camera
processKeyboard direction deltaTime c = do
  let
    velocity = speed c * deltaTime
  case direction of
    CameraForward ->
      c { position = (position c + (front c ^* velocity)) }
    CameraBackward ->
      c { position = (position c - (front c ^* velocity)) }
    CameraLeft ->
      c { position = (position c - (right c ^* velocity)) }
    CameraRight ->
      c { position = (position c + (right c ^* velocity)) }

data CameraDirection
  = CameraForward
  | CameraBackward
  | CameraLeft
  | CameraRight
  deriving (Show, Eq)
