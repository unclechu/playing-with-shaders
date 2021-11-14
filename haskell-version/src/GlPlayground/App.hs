{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GlPlayground.App
     ( runApp
     ) where

import Data.Bifunctor (bimap)
import Data.Function (fix)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.String (fromString)
import qualified Data.Text as T

import Control.Monad (unless)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Utils


runApp ∷ IO ()
runApp = do
  state@State{..} ← mkState

  window ← mkWindow $ \case
    _ → pure ()
    -- x → logInfo ∘ fromString ∘ show $ x

  renderLoop state window render (pure ())


render ∷ State → (Int, Int) → Double → IO State
render prevState@State{..} (w, h) time = do
  GL.renderPrimitive GL.Triangles $ do
    let
      anim = 0.1 × time
      x = 0.5

    GL.color $ GL.Color3 @Double 0.1 0.2 0.3
    GL.vertex $ GL.Vertex3 0 (rh $ x + anim) 0
    GL.color $ GL.Color3 @Double 0.3 0.2 0.1
    GL.vertex $ GL.Vertex3 (rw $ (-x) + (-anim)) (rh $ (-x) + (-anim)) 0
    GL.color $ GL.Color3 @Double 0.3 0.3 0.3
    GL.vertex $ GL.Vertex3 (rw $ x + anim) (rh $ (-x) + (-anim)) 0

  pure nextState

  where
    r = fromIntegral w ÷ fromIntegral h ∷ Double
    rw x | r > 1.0 = x ÷ r | otherwise = x ∷ Double
    rh x | r < 1.0 = x × r | otherwise = x ∷ Double

    nextState = prevState
      { state'LastTime = time
      }


data Event
  = Event'Key GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
  | Event'MousePos Double Double
  | Event'MouseScroll Double Double
  | Event'MouseButton GLFW.MouseButton GLFW.MouseButtonState GLFW.ModifierKeys
  deriving stock (Eq, Show)


data State
  = State
  { state'LastTime ∷ Double
  }


mkState ∷ IO State
mkState
  = State
  ∘ pure 0


mkWindow ∷ (Event → IO ()) → IO GLFW.Window
mkWindow onEventCallback = do
  logInfo "Initializing GLFW…"
  GLFW.init >>= flip unless (fail "Failed to initialize GLFW!")

  logInfo "Setting GLFW error callback…"
  GLFW.setErrorCallback ∘ Just $ \err msg →
    fail $ "GLFW reports error " ⋄ show err ⋄ ": " ⋄ msg

  logInfo "Setting minimal OpenGL version for GLFW…"
  mapM_ GLFW.windowHint
    [ GLFW.WindowHint'ContextVersionMajor 2
    , GLFW.WindowHint'ContextVersionMinor 0
    ]

  logInfo "Creating GLFW window…"
  window ←
    GLFW.createWindow 640 480 "Playing with GLSL" Nothing Nothing
      >>= maybe (fail "Failed to create GLFW window!") pure

  do -- Setting event callbacks

    logInfo "Setting GLFW key event callback…"
    GLFW.setKeyCallback window ∘ Just $ \_ key scancode keyState mods → do
      -- For some reason keys are not recognized.
      -- See: https://github.com/bsl/GLFW-b/issues/94
      keyRecognized ← GLFW.getKeyScancode GLFW.Key'Escape • (≠ (-1))
      if ( (keyRecognized ∧ key ≡ GLFW.Key'Escape)
         ∨ (key ≡ GLFW.Key'Unknown ∧ scancode ≡ 9)
         ) ∧ keyState ≡ GLFW.KeyState'Pressed
         then do
           logInfo $ T.unwords
             [ "Received escape key press event."
             , "Marking window as closing…"
             ]
           GLFW.setWindowShouldClose window True
         else onEventCallback $ Event'Key key scancode keyState mods

    logInfo "Setting GLFW mouse positioning callback…"
    GLFW.setCursorPosCallback window ∘ Just $ \_ x y →
      onEventCallback $ Event'MousePos x y

    logInfo "Setting GLFW mouse scroll callback…"
    GLFW.setCursorPosCallback window ∘ Just $ \_ x y →
      onEventCallback $ Event'MouseScroll x y

    logInfo "Setting GLFW mouse button callback…"
    GLFW.setMouseButtonCallback window ∘ Just $ \_ a b c →
      onEventCallback $ Event'MouseButton a b c

  logInfo "Making GLFW window be current OpenGL context…"
  GLFW.makeContextCurrent $ Just window

  -- TODO: init glew

  logInfo "Disabling vertical synchronization…"
  GLFW.swapInterval (-1) -- no vsync (tearing is fixed at deriver’s level)

  pure window


renderLoop
  ∷ state
  → GLFW.Window
  → (state → (Int, Int) → Double → IO state)
  → IO ()
  → IO ()
renderLoop initialState window renderFn finalizerFn = do
  logInfo "Running a render loop…"

  ($ initialState) ∘ fix $ \again prevState → do
    shouldClose ← GLFW.windowShouldClose window
    unless shouldClose $ do
      canvasSize ← GLFW.getFramebufferSize window

      GL.viewport GL.$=
        ( GL.Position 0 0
        , uncurry GL.Size
        ∘ bimap fromIntegral fromIntegral
        $ canvasSize
        )

      GL.clear [GL.ColorBuffer]
      time ← GLFW.getTime >>= maybe (fail "Failed to read current time!") pure
      nextState ← renderFn prevState canvasSize time
      GLFW.swapBuffers window
      GLFW.pollEvents -- FIXME: Do in a separate thread and use waitEvents instead
      again nextState

  logInfo "Destroying GLFW window…"
  GLFW.destroyWindow window

  logInfo "Calling finalizer callback…"
  finalizerFn

  logInfo "Terminating GLFW…"
  GLFW.terminate
