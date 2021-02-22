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

  mkvictorytext :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Congratulations!"

  mkvictorytext2 :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext2 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "You win!"

  mkvictorytext3 :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext3 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press R to replay"

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

  -- Game state win
  render game @ Game { gameState = Winner}  = pictures [ 
    mkvictorytext orange (-200) 200 0.5 0.5
    ,mkvictorytext2 orange (-100) 100 0.5 0.5
    ,mkvictorytext3 orange (-250) 0 0.5 0.5] 

  -- Playing state
  render game @ Game { gameState = Playing } =
    pictures [ball, walls, bricks,
              mkPaddle blue (player1 game) (-200)]

    where
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
            rectangleSolid brickwidth brickheight

      brickColor = aquamarine
      bricks = pictures [brick (fst x) (snd x) | x <- brickloc game]

