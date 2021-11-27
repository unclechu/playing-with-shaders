{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module GlPlayground.Boilerplate.Shaders
     ( mkShader
     , mkProgram
     , mkVertexBuffer
     ) where

import GHC.TypeLits

import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

import Control.Monad (unless)

import UnliftIO (MonadUnliftIO, MonadIO (liftIO))
import UnliftIO.Foreign (withArrayLen, Storable (sizeOf))

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Logger
import GlPlayground.TypeLevel
import GlPlayground.Types
import GlPlayground.Utils


-- This function needs "WindowContextEvidence" because if you call it before
-- window is created you get compilation failure with empty error message.
mkShader
  ∷ ∀ m t. (MonadIO m, MonadFail m, MonadLogger m, Descendible t)
  ⇒ WindowContextEvidence
  → Proxy (t ∷ GL.ShaderType)
  → T.Text
  → m (TypedShader t)
mkShader (attest → ()) (descend → shaderType) shaderSrc = do
  logInfo $ "Compiling some " ⋄ fromString (show shaderType) ⋄ "…"

  liftIO GL.shaderCompiler >>=
    flip unless (loggedFail "No shader compiler detected!")

  shader ← liftIO $ GL.createShader shaderType
  liftIO $ GL.shaderSourceBS shader GL.$=! encodeUtf8 shaderSrc
  liftIO $ GL.compileShader shader

  compilationSuccess ← liftIO $ GL.compileStatus shader
  unless compilationSuccess $ do
    errorLog ← liftIO $ GL.shaderInfoLog shader

    loggedFail $ mconcat
      [ "Failed to compile "
      , show shaderType
      , ". Shader compilation error:\n"
      , errorLog, "\n"
      , "Shader source:\n"
      , T.unpack shaderSrc
      ]

  pure $ TypedShader @t shader


mkProgram
  ∷ (MonadIO m, MonadLogger m, MonadFail m)
  ⇒ (TypedShader 'GL.VertexShader, TypedShader 'GL.FragmentShader)
  → m GL.Program
mkProgram (vertex, fragment) = do
  logInfo "Making a shader program…"
  program ← liftIO GL.createProgram

  logInfo "Attaching shaders to the program…"
  liftIO $ mapM_ (GL.attachShader program) unTypedShaders

  logInfo "Linking the shader program…"
  liftIO $ GL.linkProgram program

  linkStatus ← liftIO $ GL.linkStatus program
  unless linkStatus $
    loggedFail "Failed to link the shader program!"

  logInfo "Detaching shaders to the program…"
  liftIO $ mapM_ (GL.detachShader program) unTypedShaders

  logInfo "Validating the shader program…"
  liftIO $ GL.validateProgram program

  validationStatus ← liftIO $ GL.validateStatus program
  unless validationStatus $ do
    errorLog ← liftIO $ GL.programInfoLog program

    loggedFail $ mconcat
      [ "Failed to validate the shader program. "
      , "Error message:\n"
      , errorLog
      ]

  pure program

  where
    unTypedShaders =
      [ unTypedShader vertex
      , unTypedShader fragment
      ]


mkVertexBuffer
  ∷ ∀ m d a. (MonadUnliftIO m, Descendible d, Storable a, Num a)
  ⇒ Dimensional d [a]
  → m (VertexBuffer d)
mkVertexBuffer valuesList = do
  bufferObject ← liftIO GL.genObjectName
  liftIO $ GL.bindBuffer GL.ArrayBuffer GL.$=! Just bufferObject

  (totalSize, verticesCount) ←
    withArrayLen (unDimensional valuesList) $ \count arr → do
      let totalSize = Octets $ unOctets itemSize × count

      liftIO $ GL.bufferData GL.ArrayBuffer GL.$=!
        (fromInteger ∘ toInteger $ totalSize, arr, GL.StaticDraw)

      pure (totalSize, count `div` dimensionsToNum (descend $ Proxy @d))

  pure $ VertexBuffer
    bufferObject
    (Dimensional @d verticesCount)
    totalSize

  where
    itemSize = Octets $ sizeOf @a 0
