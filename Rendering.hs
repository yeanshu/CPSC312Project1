{-# LANGUAGE NamedFieldPuns #-}

module Rendering where

  import GameBoard

  import Graphics.Gloss

  paddleColor = blue


  --Make stateText
  mkStateText :: Color -> String -> Float -> Float -> Picture
  mkStateText col text x y = translate (-120) 0 $ scale x y $ color col $ Text text

  --Make a paddle of a given border and vertical offset.
  mkPaddle :: Color -> Float -> Float -> Picture
  mkPaddle col x y = pictures
    [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
    , translate x y $ color paddleColor $ rectangleSolid
          (paddleWidth - paddleBorder) (paddleHeight - paddleBorder)
    ]

  -- | Render game in IO
  renderIO :: (BreakoutGame -> IO Picture)
  renderIO game = return $ render game

  -- | Draw a Breakout game state (convert it to a picture).
  render :: BreakoutGame  -- ^ The game state to render
         -> Picture   -- ^ A picture of this game state

  -- Paused state
  render game @ Game { gameState = Paused } =
    mkStateText orange "PAUSED" 0.5 0.5

  -- Game Over
  render game @ Game { gameState = Over} =
    translate (-300) 0 $ scale 0.5 0.5 $ color orange $ Text "Retry? Press R"

  -- Playing state
  render game @ Game { gameState = Playing } =
    pictures [ball, walls, bricks,
              mkPaddle blue (player1 game) paddlesY]

    where

      -- State text
      stateText = "Paused"
      gameOverText = "Game Over"

      -- The Breakout ball.
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
      ballColor = dark red

      -- The bottom and top walls
      wall :: Float -> Picture
      wall offset =
        translate offset 0 $
          color wallColor $
            rectangleSolid 10 1000

      -- Left/Right walls
      walltop :: Float -> Picture
      walltop offset = 
        translate 0 offset $
          color wallColor $
            rectangleSolid 1000 10

      wallColor = greyN 0.5
      walls = pictures [wall 500, wall (-500), walltop 500, walltop (-500)]

      brick :: Float -> Float -> Picture
      brick offset offset2=
        translate offset offset2 $
          color brickColor $
            rectangleSolid 75 25

      brickColor = aquamarine
      bricks = pictures [brick (x*100) (100 + (y*50)) |  x <- [-3..3], y <- [1..4]]

