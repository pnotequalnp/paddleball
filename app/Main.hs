{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.Applicative (empty)
import Control.Monad (guard, void)
import Data.Functor (($>))
import FRP.Yampa
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Yampa (InputEvent, playYampa)
import Graphics.Gloss.Interface.Pure.Game qualified as Gloss

main :: IO ()
main = playYampa FullScreen black 60 playGame
  where
    playGame = switch (paddleBall (pi / 3) 200) $ const (switch gameOver (const playGame))

paddleBall :: Float -> Float -> SF (Event InputEvent) (Picture, Event ())
paddleBall theta v = proc inp -> do
  rec xPos <- integral -< xVel
      yPos <- integral -< yVel
      (mPos, _) <- mousePos -< inp
      let mPos' = min 200 . max (-200) $ mPos
          paddle = translate mPos' (-200) . color white $ rectangleSolid 100 10
          ball = translate xPos yPos . color red $ circleSolid 10
          walls = color white $ rectangleWire 500 500
          paddleHit = inRect (mPos' + 50, -190) (mPos' - 50, -200) (xPos, yPos -10)
          miss = guard $ yPos <= (-240)
          scoreCard = color white . translate (-25) 260 . scale 0.1 0.1 . text $ "Score: " <> show score
      xCollision <- edge -< xPos >= 240 || xPos <= (-240)
      yCollision <- edge -< yPos >= 240 || paddleHit
      xVel <- accumHold (v * cos theta) -< xCollision $> negate
      yVel <- accumHold (v * sin theta) -< yCollision $> negate
      score <- accumHold (0 :: Int) -< guard paddleHit $> succ
  returnA -< (pictures [paddle, ball, walls, scoreCard], miss)

gameOver :: SF (Event InputEvent) (Picture, Event ())
gameOver = proc inp -> do
  let msg = color red . translate (-110) 30 . scale 0.3 0.3 $ text "Game Over"
  (b, restart) <- restartButton (0, 0) -< inp
  returnA -< (pictures [msg, b], restart)

restartButton :: Point -> SF (Event InputEvent) (Picture, Event ())
restartButton c@(cx, cy) = proc inp -> do
  clicked <- click -< inp
  returnA -< (image, void . filterE inBounds $ clicked)
  where
    w = 100
    h = 25
    inBounds (x, y) = x < cx + (w / 2) && x > cx - (w / 2) && y < cy + (h / 2) && y > cy - (h / 2)
    image = pictures [label, box]
    label = color white . translate (-30) (-7) . scale 0.15 0.15 $ text "Restart"
    box = color white . uncurry translate c $ rectangleWire w h

click :: SF (Event InputEvent) (Event (Float, Float))
click = arr (>>= go)
  where
    go = \case
      Gloss.EventKey (Gloss.MouseButton Gloss.LeftButton) _ _ x -> pure x
      _ -> empty

inRect :: Point -> Point -> Point -> Bool
inRect (x1, y1) (x2, y2) (x, y) = x2 <= x && x <= x1 && y2 <= y && y <= y1

mousePos :: SF (Event InputEvent) (Float, Float)
mousePos = hold (0, 0) <<< arr (>>= getPos)
  where
    getPos = \case
      Gloss.EventMotion x -> Event x
      _ -> NoEvent
