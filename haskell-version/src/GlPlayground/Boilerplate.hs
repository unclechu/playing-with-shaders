{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Boilerplate
     ( mkWindow
     , mainLoop
     ) where

import Data.Bifunctor (bimap)
import Data.String (fromString)
import qualified Data.Text as T

import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger)

import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Async (async)
import UnliftIO.Exception (SomeException, catch)
import UnliftIO.IORef (IORef, newIORef, readIORef, atomicModifyIORef')

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Logger
import GlPlayground.Types
import GlPlayground.Utils


mkWindow
  ∷ ∀ m. (MonadUnliftIO m, MonadLogger m, MonadFail m)
  ⇒ (Event → m ())
  → m GLFW.Window
mkWindow onEventCallback = do
  logInfo "Initializing GLFW…"
  liftIO GLFW.init >>= flip unless (fail "Failed to initialize GLFW!")

  logInfo "Setting GLFW error callback…"
  withRunInIO $ \runInIO →
    GLFW.setErrorCallback ∘ Just $ \err msg → runInIO $ do
      logError ∘ fromString $ "GLFW reports error " ⋄ show err ⋄ ": " ⋄ msg
      fail $ "GLFW reports error " ⋄ show err ⋄ ": " ⋄ msg

  logInfo "Setting minimal OpenGL version for GLFW…"
  mapM_ (liftIO ∘ GLFW.windowHint)
    [ GLFW.WindowHint'ContextVersionMajor 2
    , GLFW.WindowHint'ContextVersionMinor 0
    ]

  logInfo "Creating GLFW window…"
  window ←
    liftIO (GLFW.createWindow 640 480 "Playing with GLSL" Nothing Nothing)
      >>= maybe (fail "Failed to create GLFW window!") pure

  do -- Setting event callbacks

    logInfo "Setting GLFW key event callback…"
    withRunInIO $ \runInIO → do
      -- For some reason keys are not recognized.
      -- See: https://github.com/bsl/GLFW-b/issues/94
      hasLayoutRef ← newIORef (Nothing @Bool)

      GLFW.setKeyCallback window ∘ Just $
        \_ originalKey scancode keyState mods → runInIO $ do
          key ← keyLayoutResolve hasLayoutRef originalKey scancode

          if key ≡ GLFW.Key'Escape ∧ keyState ≡ GLFW.KeyState'Pressed
             then do
               logInfo $ T.unwords
                 [ "Received escape key press event."
                 , "Marking window as closing…"
                 ]
               liftIO $ GLFW.setWindowShouldClose window True
             else
               onEventCallback $ Event'Key key scancode keyState mods

    logInfo "Setting GLFW mouse positioning callback…"
    withRunInIO $ \runInIO →
      GLFW.setCursorPosCallback window ∘ Just $ \_ x y →
        runInIO ∘ onEventCallback $ Event'MousePos x y

    logInfo "Setting GLFW mouse scroll callback…"
    withRunInIO $ \runInIO →
      GLFW.setCursorPosCallback window ∘ Just $ \_ x y →
        runInIO ∘ onEventCallback $ Event'MouseScroll x y

    logInfo "Setting GLFW mouse button callback…"
    withRunInIO $ \runInIO →
      GLFW.setMouseButtonCallback window ∘ Just $ \_ a b c →
        runInIO ∘ onEventCallback $ Event'MouseButton a b c

    logInfo "Setting GLFW framebuffer resize callback…"
    withRunInIO $ \runInIO →
      GLFW.setFramebufferSizeCallback window $ Just $ \_ w h →
        runInIO ∘ onEventCallback $ Event'CanvasResize w h

  logInfo "Dispatching initial framebuffer resize event…"
  withRunInIO $ \runInIO →
    GLFW.getFramebufferSize window >>=
      runInIO ∘ onEventCallback ∘ uncurry Event'CanvasResize

  logInfo "Making GLFW window be current OpenGL context…"
  liftIO ∘ GLFW.makeContextCurrent $ Just window

  -- TODO: init glew

  -- No vsync (tearing is fixed at deriver’s level)
  logInfo "Disabling vertical synchronization…"
  liftIO $ GLFW.swapInterval (-1)

  pure window

  where
    keyLayoutResolve ∷ IORef (Maybe Bool) → GLFW.Key → Int → m GLFW.Key
    keyLayoutResolve hasLayoutRef key scancode =
      let
        fromDefaultLayout ∷ GLFW.Key
        fromDefaultLayout = case scancode of
          9 → GLFW.Key'Escape
          _ → key
      in
        readIORef hasLayoutRef >>= \case
          Just True → pure key
          Just False → pure fromDefaultLayout
          Nothing → do
            isRecognized ←
              liftIO $ GLFW.getKeyScancode GLFW.Key'Escape • (≠ (-1))

            _ ← async $
              let
                errHandler (e ∷ SomeException) =
                  logError $ "Background task failed: " ⋄ fromString (show e)

                task = do
                  wasUpdated ←
                    atomicModifyIORef' hasLayoutRef $
                      maybe (Just isRecognized, True) (\x → (Just x, False))

                  when (wasUpdated ∧ not isRecognized) $
                    logWarning $ T.unwords
                      [ "It seems that there is a problem with recognizing"
                      , "keyboard layout (couldn’t get scan code of Escape"
                      , "key). Default QWERTY layout is used instead."
                      ]
              in
                task `catch` errHandler

            pure $ if isRecognized then key else fromDefaultLayout


mainLoop
  ∷ (MonadUnliftIO m, MonadLogger m, MonadFail m, HasCanvasSize state)
  ⇒ GLFW.Window
  → state
  -- ^ Initial state
  → (state → m state)
  -- ^ State update callback
  → (state → m ())
  -- ^ Render callback
  → m ()
  -- ^ Finalizer callback
  → m ()
mainLoop window initialState updateFn renderFn finalizerFn = do
  logInfo "Running a render loop…"

  ($ initialState) ∘ fix $ \again state → do
    shouldClose ← liftIO $ GLFW.windowShouldClose window
    unless shouldClose $ do
      nextState ← updateFn state

      let
        lastCanvasSize = getOldCanvasSize state
        canvasSize = getNewCanvasSize state

      unless (canvasSize ≡ lastCanvasSize) $
        liftIO $ GL.viewport GL.$=
          ( GL.Position 0 0
          , uncurry GL.Size
          ∘ bimap fromIntegral fromIntegral
          $ canvasSize
          )

      liftIO $ GL.clear [GL.ColorBuffer]
      renderFn state
      liftIO $ GLFW.swapBuffers window
      liftIO GLFW.pollEvents
      again nextState

  logInfo "Destroying GLFW window…"
  liftIO $ GLFW.destroyWindow window

  logInfo "Calling finalizer callback…"
  finalizerFn

  logInfo "Terminating GLFW…"
  liftIO GLFW.terminate
