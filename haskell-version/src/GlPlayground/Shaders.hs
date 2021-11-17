{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Shaders
     ( mandelbrotSetShaderProgram
     ) where

import Data.Data (Proxy (Proxy))
import Data.FileEmbed (embedStringFile)

import UnliftIO (MonadUnliftIO)

import qualified Graphics.Rendering.OpenGL.GL.Shaders as GLSL

import GlPlayground.Boilerplate
import GlPlayground.Logger
import GlPlayground.Types


mandelbrotSetShaderProgram
  ∷ (MonadUnliftIO m, MonadFail m, MonadLogger m)
  ⇒ WindowContextEvidence
  → m GLSL.Program
mandelbrotSetShaderProgram evidence' = do
  logInfo "Making Mandelbrot set shader program…"

  vertex ←
    mkShader
      evidence'
      (Proxy @'GLSL.VertexShader)
      $(embedStringFile "shaders/mandelbrot-set/vertex.glsl")

  fragment ←
    mkShader
      evidence'
      (Proxy @'GLSL.FragmentShader)
      $(embedStringFile "shaders/mandelbrot-set/fragment.glsl")

  mkProgram (vertex, fragment)
