{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.Applicative (empty)
import Control.Monad (guard)
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
      xCollision <- edge -< xPos >= 240 || xPos <= (-240)
      yCollision <- edge -< yPos >= 240 || paddleHit
      xVel <- accumHold (v * cos theta) -< xCollision $> negate
      yVel <- accumHold (v * sin theta) -< yCollision $> negate
  returnA -< (pictures [paddle, ball, walls], miss)

gameOver :: SF (Event InputEvent) (Picture, Event ())
gameOver = proc inp -> do
  let msg = color red . scale 0.3 0.3 $ text "Game Over"
      restart = inp >>= clicked
  returnA -< (msg, restart)
  where
    clicked :: InputEvent -> Event ()
    clicked = \case
      Gloss.EventKey (Gloss.MouseButton Gloss.LeftButton) _ _ _ -> pure ()
      _ -> empty

inRect :: Point -> Point -> Point -> Bool
inRect (x1, y1) (x2, y2) (x, y) = x2 <= x && x <= x1 && y2 <= y && y <= y1

mousePos :: SF (Event InputEvent) (Float, Float)
mousePos = hold (0, 0) <<< arr (>>= getPos)
  where
    getPos = \case
      Gloss.EventMotion x -> Event x
      _ -> NoEvent
