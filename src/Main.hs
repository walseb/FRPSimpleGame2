{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

import Actor
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import FRP.Yampa
import FRPEngine.Collision.GJK
import FRPEngine.Init
import FRPEngine.Input.Input
import FRPEngine.Input.Types as I
import FRPEngine.Types
import Level
import Render.SDL.Render
import qualified SDL as S
import qualified SDL.Font as F
import SDL.Image as SI
import System.IO.Unsafe
import Types

runPhysical :: PhysicalState -> SF InputState PhysicalState
runPhysical (PhysicalState (CollObj iPC iP) iE) =
  proc input -> do
    p <- playerRun iP -< input
    returnA -< PhysicalState (CollObj iPC p) iE

run :: GameState -> SF InputState (GameState, Event GameState)
run orig@(GameState (CameraState iZ) p alive) =
  proc input -> do
    physical <- runPhysical p -< input
    alive <- collidedDeathSwitch -< physical
    returnA -<
      ( ( GameState
            (CameraState iZ)
            physical
            alive
        ),
        if alive then NoEvent else Event orig
      )

runDeathResetSwitch :: GameState -> SF InputState GameState
runDeathResetSwitch game =
  switch
    (run game)
    runDeathResetSwitch

collided :: SF PhysicalState (Bool, Event ())
collided = proc (PhysicalState player enemies) -> do
  let hasCollided =
        or (collidesObj player <$> enemies)
  returnA -<
    ( not hasCollided,
      case hasCollided of
        True -> Event ()
        False -> NoEvent
    )

collidedDeathSwitch :: SF PhysicalState Bool
collidedDeathSwitch =
  switch
    collided
    (\_ -> constant False)

update :: GameState -> MVar GameState -> SF (Event [S.Event]) (GameState, Bool)
update origGameState mvar =
  proc events -> do
    newInputState <- accumHoldBy inputStateUpdate defaultKeybinds -< events
    gameState <- runDeathResetSwitch origGameState -< newInputState
    let quit = (fromJust (newInputState ^. I.quit ^? pressed))
    let quit' =
          if quit
            then seq (unsafePerformIO (putMVar mvar gameState)) True
            else False
    returnA -<
      ( gameState,
        quit'
      )

loadOldGameState :: Maybe GameState -> MVar GameState -> SF (Event [S.Event]) (GameState, Bool)
loadOldGameState Nothing =
  update initialGame
loadOldGameState (Just origGameState) =
  update origGameState

getResources :: (MonadIO m) => S.Renderer -> m Resources
getResources renderer =
  -- Init fonts
  Resources
    <$> (F.initialize >> F.load fontPath 12)
    <*> load spritePath renderer
    <*> load spritePath2 renderer
  where
    load :: (MonadIO m) => FilePath -> S.Renderer -> m S.Texture
    load path rend = SI.loadTexture rend path
    fontPath = "data/fonts/OpenSans-Regular.ttf"
    spritePath = "data/enemy.png"
    spritePath2 = "data/player.png"

main = do
  myMVar <- newEmptyMVar
  runSDL
    True
    S.Windowed
    "FRP Lunar Lander"
    getResources
    ( \savedGameState renderer senseInput resources -> do
        _ <- reactimate (pure NoEvent) senseInput (\_ -> render renderer resources) (loadOldGameState savedGameState myMVar)
        mvar <- takeMVar myMVar
        pure mvar
    )
