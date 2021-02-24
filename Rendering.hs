{-# LANGUAGE NamedFieldPuns #-}

module Rendering where

  import GameBoard

  import Graphics.Gloss

  paddleColor = blue

  data Library = Library
    { bgImg :: Picture
    }

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

  --Make victory messages
  mkvictorytext :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Congratulations!"

  mkvictorytext2 :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext2 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "You win!"

  mkvictorytext3 :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext3 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press R to replay"

  --Make Title Screen messages
  mktitle1 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle1 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "BREAKOUT"

  mktitle2 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle2 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "by Yean, Jason, and Daniel"

  mktitle3 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle3 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press x to play"

  type Archive = (BreakoutGame, Picture)
  
  -- | Render game in IO
  renderIO :: (Archive -> IO Picture)
  renderIO (game, bg) = return $ render game bg

  -- | Draw a Breakout game state (convert it to a picture).
  render :: BreakoutGame  -- ^ The game state to render
         -> Picture   -- Background image
         -> Picture   -- ^ A picture of this game state

  -- Title Screen
  render game @ Game { gameState = Title} bg = pictures [
    mktitle1 white (-375) 300 1.2 1.5
    ,mktitle2 white (-260) 150 0.3 0.3
    ,mktitle3 white (-250) (-400) 0.5 0.5]

  -- Paused state
  render game @ Game { gameState = Paused } bg =
    mkStateText orange "PAUSED" 0.5 0.5

  -- Game Over
  render game @ Game { gameState = Over} bg =
    translate (-400) 0 $ scale 0.5 0.5 $ color orange $ Text "Retry? Press R"

  -- Game state win
  render game @ Game { gameState = Winner} bg = pictures [ 
    mkvictorytext orange (-450) 200 1.0 1.0
    ,mkvictorytext2 orange (-150) 50 0.5 0.5
    ,mkvictorytext3 orange (-300) (-300) 0.5 0.5] 

  -- Playing state
  render game @ Game { gameState = Playing } bg=
    pictures [bg, ball, walls, bricks,
              mkPaddle blue (player1 game) (-200), scoreRender (score game)]

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

      scoreRender :: Int -> Picture
      scoreRender score = translate 510 450 $ scale 0.15 0.15 $ color orange $ Text ("Score: " ++ (show score))
