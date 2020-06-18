module Render.SDL.Render where

import Control.Lens
import Linear
import FRPEngine.Render.SDL.Primitives
import qualified SDL as S
import FRPEngine.Types
import Types
import Control.Monad

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer res (GameState (CameraState zoomLevel) (PhysicalState player enemies) alive, exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 0 255
    S.clear renderer

    when alive $ renderSpr (player ^. (collObj . obj))

    sequence_ $ renderSpr . (^. (collObj . obj)) <$> enemies

    -- sequence_ $ (join . join) $ (fmap . fmap . fmap) renderPt $ getCollisionPointsPos <$> [player]
    -- sequence_ $ (join . join) $ (fmap . fmap . fmap) renderPt $ getCollisionPointsPos <$> enemies

    S.present renderer
    pure exit
  where
    -- Static stuff center rot at top left
    renderObj' = renderObj (player ^. (collObj . obj . pos)) (flip getSprite res) (fromIntegral zoomLevel) renderer
    renderSpr = renderObj'
    -- renderTerr = renderObj' False
    -- renderText' = renderText renderer (res ^. font)
    renderPt pos = renderObj' (Obj pos 0 0 (V2 50 50) SobjectSprite2 True)
