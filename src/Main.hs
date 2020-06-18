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
import FRPEngine.Physics.Collision.GJK
import FRPEngine.Init
import FRPEngine.Input.Input
import FRPEngine.Input.Types as I
import FRPEngine.Input.Utils
import FRPEngine.Types
import Level
import Linear
import Render.SDL.Render
import qualified SDL as S
import qualified SDL.Font as F
import SDL.Image as SI
import System.IO.Unsafe
import Types
import Input

runPhysical :: PhysicalState -> SF [Input] PhysicalState
runPhysical state@(PhysicalState (StretchCollObj origSize (CollObj iPC iP)) iE) =
  proc input -> do
    p <- playerRun iP origSize -< input
    returnA -< (player . collObj . obj) .~ p $ state

run :: GameState -> SF [Input] (GameState, Event GameState)
run (GameState (CameraState iZ) p alive) = proc input -> do
  physical <- runPhysical p -< input
  alive <- collidedDeathSwitch -< physical
  returnA -<
    ( ( GameState
          (CameraState iZ)
          physical
          alive
      ),
      if alive then NoEvent else Event initialGame
    )

runDeathResetSwitch :: GameState -> SF [Input] GameState
runDeathResetSwitch game =
  switch
    (run game)
    runDeathResetSwitch

collided :: SF PhysicalState (Bool, Event ())
collided = proc (PhysicalState player enemies) -> do
  let hasCollided =
        or (collidesObj (player ^. collObj) <$> ((^. collObj) <$> enemies))
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
    newInputState <- accumHoldBy updateInput keybinds -< events
    gameState <- runDeathResetSwitch origGameState -< newInputState
    let quit = quitKey newInputState
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
    S.Maximized
    "Simple Game 2"
    getResources
    ( \savedGameState renderer senseInput resources -> do
        _ <- reactimate (pure NoEvent) senseInput (\_ -> render renderer resources) (loadOldGameState savedGameState myMVar)
        mvar <- takeMVar myMVar
        pure mvar
    )
