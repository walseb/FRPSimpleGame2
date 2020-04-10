{-# LANGUAGE Arrows #-}

module Actor where

import Control.Lens
import Data.Coerce
import FRP.Yampa
import FRPEngine.Input.Interpreter (vectorizeMovement)
import FRPEngine.Input.Types
import FRPEngine.Types
import FRPEngine.YampaUtils.Types ()
import Linear

speed :: (Num a) => V2 a
speed = V2 1000 500

sizeSpeed :: (RealFrac a) => V2 a
sizeSpeed = 0.05

type MoveKeys a = V2 a

type Pos a = V2 a
type Size a = V2 a
type Vel a = V2 a

move :: (RealFloat a) => (Pos a, Size a) -> SF (MoveKeys a) (Pos a, Size a)
move (V2 iPX iPY, size) = proc dir -> do
  -- Pos
  vel <- moveNoBackSwitch 0 -< dir
  posX' <- integralFrom (V1 iPX) -< V1 (vel ^. _x)
  posY <- integralFrom (V1 iPY) -< V1 (vel ^. _y)

  -- Size
  size <- sizeRun size -< vel

  returnA -<
    let pos = coerce <$> V2 posX' posY
     in (pos, size)

moveVel :: (RealFloat a) => Vel a -> SF (MoveKeys a) (Vel a, Event (Vel a))
moveVel initVel = proc dir -> do
  vel <- integralFrom initVel -< (dir * speed)
  returnA -< (vel, if (vel ^. _x) < 0 then Event (V2 0 (vel ^. _y)) else NoEvent)

moveNoBackSwitch :: (RealFloat a) => Vel a -> SF (MoveKeys a) (Vel a)
moveNoBackSwitch initVel =
  switch
    (moveVel initVel)
    moveNoBackSwitch

sizeRun :: (RealFloat a) => Size a -> SF (V2 a) (Size a)
sizeRun size@(V2 sizeX sizeY) = proc vel -> do
  let vel' = (V2 (vel ^. _x) ((vel ^. _y) - (vel ^. _x)))
  returnA -< (size / 2) + (keepMinimumSize <$> ((size / 2) + (vel' * sizeSpeed)))
    where
      keepMinimumSize a = if a < 0 then 0 else a

playerRun :: (RealFloat a) => Obj a _b -> SF InputState (Obj a _b)
playerRun initObj = proc input -> do
  (pos', size') <- move (initObj ^. pos, initObj ^. size) -< vectorizeMovement (input ^. movement)
  returnA -< (pos .~ pos') . (size .~ size') $ initObj
