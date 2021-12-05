{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Game.MandelbrotSet.ShaderProgram
     ( shaderProgram
     ) where

import Data.Data (Proxy (Proxy))
import Data.FileEmbed (embedStringFile)

import UnliftIO (MonadUnliftIO)

import qualified Graphics.Rendering.OpenGL.GL.Shaders as GLSL

import GlPlayground.Boilerplate.Shaders
import GlPlayground.Logger
import GlPlayground.Types


shaderProgram
  ∷ (MonadUnliftIO m, MonadFail m, MonadLogger m)
  ⇒ WindowContextEvidence
  → m GLSL.Program
shaderProgram evidence' = do
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
