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
import Control.Applicative

speed :: (Num a) => V2 a
speed = (V2 1000 500)

sizeSpeed :: (RealFrac a) => V2 a
sizeSpeed = 0.05

minSize :: (RealFrac a) => V2 a
minSize = (V2 100 100)

-- sizeDeteriorationCoefficiant :: (RealFrac a) => V2 a
-- sizeDeteriorationCoefficiant = (V2 2 2)

sizeDeteriorationSpeed :: (RealFrac a) => V2 a
sizeDeteriorationSpeed = (V2 20 60)

maxX :: (RealFloat a) => a
maxX = 527
-- maxX = 500

minX :: (RealFloat a) => a
minX = 0
-- minX = 0

type MoveKeys a = V2 a

type Pos a = V2 a
type Size a = V2 a

move :: (RealFloat a) => (Pos a, Size a) -> SF (MoveKeys a) ((Pos a, Size a), Event (Pos a, Size a))
move ((V2 iPX iPY), size) = proc dir -> do
  -- Pos
  vel <- integralFrom 0 -< (dir * speed)
  posX' <- integralFrom (V1 iPX) -< V1 (vel ^. _x)
  posY <- integralFrom (V1 iPY) -< V1 (vel ^. _y)

  -- Size
  -- let sizeY' = sizeX + coerce velX
  size <- sizeSwitch size -< vel

  returnA -<
    let pos = coerce <$> V2 posX' posY
     in ((pos, size), restrict pos size)
  where
    restrict (V2 x y) size
      | y > maxX = Event (V2 x maxX, size)
      | y < minX = Event (V2 x minX, size)
      | otherwise = NoEvent

sizeSwitch :: (RealFloat a) => Size a -> SF (MoveKeys a) (Size a)
sizeSwitch initSize =
  switch
    (sizeRun initSize)
    sizeSwitch

sizeRun :: (RealFloat a) => Size a -> SF (V2 a) (Size a, Event (Size a))
sizeRun size@(V2 sizeX sizeY) = proc vel -> do
  let vel' = minSize
  size' <- integralFrom size -<  (abs (vel * sizeSpeed) - (sizeDeteriorationSpeed))

  let sizeLimited = liftA2 limit size' minSize

  -- ok so basically I want to make it shrink a lot faster at lower speeds and slower at higher speeds
  -- use an online function plotter to figure out the general equation

  returnA -< (size', if sizeLimited == size' then NoEvent else Event sizeLimited)
    where
      limit :: (Num a, Ord a) => a -> a -> a
      limit a b | a < b = b
                | otherwise = a

moveSwitch :: (RealFloat a) => (Pos a, Size a) -> SF (MoveKeys a) (Pos a, Size a)
moveSwitch (initPos, initSize) =
  switch
    (move (initPos, initSize))
    moveSwitch

playerRun :: (RealFloat a) => Obj a _b -> SF InputState (Obj a _b)
playerRun initObj = proc input -> do
  (pos', size') <- moveSwitch (initObj ^. pos, initObj ^. size) -< vectorizeMovement (input ^. movement)
  returnA -< (pos .~ pos') . (size .~ size') $ initObj
