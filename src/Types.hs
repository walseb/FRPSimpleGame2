{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import SDL as S
import SDL.Font (Font)
import FRPEngine.Types

data Resources
  = Resources
      { _font :: Font,
        _objectSprite :: S.Texture,
        _objectSprite2 :: S.Texture
      }

makeLenses ''Resources

data SpriteSelect
  = Sfont
  | SobjectSprite
  | SobjectSprite2
  deriving (Show)

getSprite :: Obj a SpriteSelect -> Resources -> S.Texture
getSprite obj =
  case (obj ^. spr) of
    SobjectSprite -> _objectSprite
    SobjectSprite2 -> _objectSprite2

data CameraState
  = CameraState
      { _zoomLevel :: Int
      }
  deriving (Show)

data PhysicalState
  = PhysicalState
  {
    player :: CollObj Double SpriteSelect,
    enemies :: [CollObj Double SpriteSelect]
  }
  deriving (Show)

makeLenses ''PhysicalState

data GameState
  = GameState
      { _cameraState :: CameraState,
        _physicalState :: PhysicalState,
        _alive :: Bool
      }
  deriving (Show)

makeLenses ''GameState
