{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module GlPlayground.Boilerplate.Shaders
     ( mkShader
     , mkProgram
     , mkVertexBuffer
     , mkKnownVertexBuffer
     , getAttribLocation
     , getUniformLocation
     , setUniformValue
     ) where

import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

import Control.Monad (unless)

import UnliftIO (MonadUnliftIO, MonadIO (liftIO))
import UnliftIO.Foreign (Storable (sizeOf), withArrayLen)

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Logger
import GlPlayground.TypeLevel
import GlPlayground.TypeLevel.MemSizeOf
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
mkVertexBuffer dimensionalValuesList = do
  bufferObject ← liftIO GL.genObjectName
  liftIO $ GL.bindBuffer GL.ArrayBuffer GL.$=! Just bufferObject

  (totalSize, verticesCount) ←
    withArrayLen valuesList $ \count arr → do
      let totalSize = Octets $ unOctets itemSize × count

      liftIO $ GL.bufferData GL.ArrayBuffer GL.$=!
        (fromInteger ∘ toInteger $ totalSize, arr, GL.StaticDraw)

      pure (totalSize, count `div` dimensionsToNum (descend $ Proxy @d))

  pure $ VertexBuffer
    bufferObject
    (Dimensional @d verticesCount)
    totalSize

  where
    valuesList = unDimensional dimensionalValuesList
    itemSize = sizeOfItem valuesList


-- | Make statically-known vertex buffer
--
-- Take the data from type-level and calculate the size into a type-level
-- number. Also prove that the total amount of items is multiple of the amount
-- of dimensions.
mkKnownVertexBuffer
  ∷ ∀ m a as values d verticesCount len .
  ( MonadUnliftIO m
  , Storable as
  , DescendibleAs values [as]
  , DescendibleAs len Int
  , len ~ Length values
  , verticesCount ~ VerticesCount d values
  , VerticesCountConstraint d values
  )
  ⇒ Proxy '(d ∷ Dimensions, as ∷ Type, values ∷ [a])
  → m (KnownVertexBuffer d verticesCount)
mkKnownVertexBuffer Proxy = do
  bufferObject ← liftIO GL.genObjectName
  liftIO $ GL.bindBuffer GL.ArrayBuffer GL.$=! Just bufferObject

  totalSize ←
    withArrayLen valuesList $ \_ arr → do
      let totalSize = Octets $ unOctets itemSize × descendAs (Proxy @len)

      liftIO $ GL.bufferData GL.ArrayBuffer GL.$=!
        (fromInteger ∘ toInteger $ totalSize, arr, GL.StaticDraw)

      pure totalSize

  pure $ KnownVertexBuffer
    { knownVertexBuffer'BufferObject = bufferObject
    , knownVertexBuffer'SizeInOctets = totalSize
    }

  where
    valuesList = descendAs $ Proxy @values ∷ [as]
    itemSize = sizeOfItem valuesList


getAttribLocation
  ∷ ∀ name t n m
  . (GetMemSize t ~ 'TOctets n, DescendibleAs name String, MonadIO m)
  ⇒ GL.Program
  → m (TypedAttribLocation name t)
getAttribLocation program
  = fmap TypedAttribLocation
  ∘ liftIO
  ∘ GL.get
  ∘ GL.attribLocation program
  $ descendAs (Proxy @name)


getUniformLocation
  ∷ ∀ name t n m
  . (GetMemSize t ~ 'TOctets n, DescendibleAs name String, MonadIO m)
  ⇒ GL.Program
  → m (TypedUniformLocation name t)
getUniformLocation program
  = fmap TypedUniformLocation
  ∘ liftIO
  ∘ GL.uniformLocation program
  $ descendAs (Proxy @name)


setUniformValue
  ∷ ∀ name t n m
  . (GetMemSize t ~ 'TOctets n, GL.Uniform t, MonadIO m)
  ⇒ TypedUniformLocation name t
  → t
  → m ()
setUniformValue (TypedUniformLocation l) x =
  liftIO $ GL.uniform l GL.$=! x


-- * Helpers

sizeOfItem ∷ ∀ a. Storable a ⇒ [a] → Octets
sizeOfItem [] = Octets 0
sizeOfItem (x : _) = Octets $ sizeOf x
