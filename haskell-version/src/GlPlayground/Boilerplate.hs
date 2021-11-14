{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Boilerplate
     ( mkWindow
     , renderLoop
     ) where

import Data.Bifunctor (bimap)
import qualified Data.Text as T

import Control.Monad (unless)
import Control.Monad.Fix (fix)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Types
import GlPlayground.Utils


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

    logInfo "Setting GLFW framebuffer resize callback…"
    GLFW.setFramebufferSizeCallback window $ Just $ \_ w h →
      onEventCallback $ Event'CanvasResize w h

  logInfo "Dispatching initial framebuffer resize event…"
  GLFW.getFramebufferSize window >>=
    onEventCallback ∘ uncurry Event'CanvasResize

  logInfo "Making GLFW window be current OpenGL context…"
  GLFW.makeContextCurrent $ Just window

  -- TODO: init glew

  logInfo "Disabling vertical synchronization…"
  GLFW.swapInterval (-1) -- no vsync (tearing is fixed at deriver’s level)

  pure window


renderLoop
  ∷ HasCanvasSize state
  ⇒ state
  → GLFW.Window
  → (state → (Int, Int) → Double → IO state)
  → IO ()
  → IO ()
renderLoop initialState window renderFn finalizerFn = do
  logInfo "Running a render loop…"

  ($ initialState) ∘ fix $ \again state → do
    shouldClose ← GLFW.windowShouldClose window
    unless shouldClose $ do
      canvasSize ← getNextCanvasSize state

      unless (canvasSize ≡ getPrevCanvasSize state) $
        GL.viewport GL.$=
          ( GL.Position 0 0
          , uncurry GL.Size
          ∘ bimap fromIntegral fromIntegral
          $ canvasSize
          )

      GL.clear [GL.ColorBuffer]
      time ← GLFW.getTime >>= maybe (fail "Failed to read current time!") pure
      nextState ← renderFn state canvasSize time
      GLFW.swapBuffers window
      GLFW.pollEvents -- FIXME: Do in a separate thread and use waitEvents instead
      again nextState

  logInfo "Destroying GLFW window…"
  GLFW.destroyWindow window

  logInfo "Calling finalizer callback…"
  finalizerFn

  logInfo "Terminating GLFW…"
  GLFW.terminate
