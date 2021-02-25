{-# LANGUAGE NamedFieldPuns #-}

module Rendering where

  import GameBoard

  import Graphics.Gloss

  -- Following make-text functions displays a Picture of the text given
  -- the color of text, x and y position of text and the scale of the text (x and y)

  --Make victory messages
  mkvictorytext :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Congratulations!"

  mkvictorytext2 :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext2 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "You win!"

  mkvictorytext3 :: Color -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext3 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press R to replay"

  mkvictorytext4 :: Color -> Float -> Float -> Float -> Float -> Float  -> Picture
  mkvictorytext4 col x y scalex scaley time = translate x y $ scale scalex scaley $ color col $ Text ("Time: " ++ (show $ round (time)))

  mkvictorytext5 :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
  mkvictorytext5 col x y scalex scaley fastest = translate x y $ scale scalex scaley $ color col $ Text ("Your fastest time is: " ++ (show $ round (fastest)))

  --Make Title Screen messages
  mktitle1 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle1 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "BREAKOUT"

  mktitle2 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle2 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "by Yean, Jason, and Daniel"

  mktitle3 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle3 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press x for standard mode"

  mktitle4 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle4 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press z for easy mode"

  mktitle5 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle5 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press c for hard mode"

  mktitle6 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle6 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press v for insane mode"

  mktitle7 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktitle7 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Press b to toggle time limit mode"

  mktimetoggletext :: Color -> Float -> Float -> Float -> Float -> Picture
  mktimetoggletext col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Time limit mode: ON"

  mktimetoggletext2 :: Color -> Float -> Float -> Float -> Float -> Picture
  mktimetoggletext2 col x y scalex scaley = translate x y $ scale scalex scaley $ color col $ Text "Time limit mode: OFF"
  
  mktimedisplay :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
  mktimedisplay col x y scalex scaley time = translate x y $ scale scalex scaley $ color col $ Text ("Fastest Time: " ++ (show $ round (time)))

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
    bg
    ,mktitle1 white (-375) 300 1.2 1.5
    ,mktitle2 white (-260) 150 0.3 0.3
    ,mktitle3 white (-250) (-350) 0.2 0.2
    ,mktitle4 white (-250) (-390) 0.2 0.2
    ,mktitle5 white (-250) (-430) 0.2 0.2
    ,mktitle6 red (-250) (-470) 0.2 0.2
    ,mktitle7 white (-250) (-300) 0.2 0.2
    ,timermodetoggle white (-250) (-200) 0.2 0.2
    ,mktimedisplay white 100 250 0.2 0.2 (fastesttime game)]

    where 
      timermodetoggle col x y scalex scaley= 
        if timemodeon game 
          then mktimetoggletext col x y scalex scaley 
          else mktimetoggletext2 col x y scalex scaley



  -- Paused state
  render game @ Game { gameState = Paused } bg =
    translate (-400) 0 $ scale 0.8 0.8 $ color orange $ Text "Paused"

  -- Game Over
  render game @ Game { gameState = Over} bg =
    translate (-400) 0 $ scale 0.5 0.5 $ color orange $ Text "Retry? Press R"

  -- Game state win
  render game @ Game { gameState = Winner} bg = pictures [ 
    mkvictorytext orange (-450) 200 1.0 1.0
    ,mkvictorytext2 orange (-150) 50 0.5 0.5
    ,mkvictorytext3 orange (-300) (-300) 0.5 0.5
    ,mkvictorytext4 orange (-300) (-375) 0.5 0.5 timetaken
    ,mkvictorytext5 orange (-300) (-450) 0.5 0.5 fastest]

    where
      timetaken = victorytime game 
      fastest = fastesttime game

  -- Playing state
  render game @ Game { gameState = Playing } bg=
    pictures [bg, ball, walls, bricks,
              mkPaddle blue (player1 game) (-200), scoreRender (score game), timeLimitRender (time game)]

    where
      -- The Breakout ball.
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
      ballColor = dark red
      
        --Make a paddle of a given border and vertical offset.
      mkPaddle :: Color -> Float -> Float -> Picture
      mkPaddle col x y = translate x y $ color col $ rectangleSolid paddleWidth paddleHeight

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

      --Brick textures
      brick :: Float -> Float -> Picture
      brick offset offset2=
        translate offset offset2 $
          color brickColor $
            rectangleSolid brickwidth brickheight

      brickColor = aquamarine
      bricks = pictures [brick (fst x) (snd x) | x <- brickloc game]
      
      --Scoreboard texture
      scoreRender :: Int -> Picture
      scoreRender score = translate 510 450 $ scale 0.15 0.15 $ color orange $ Text ("Score: " ++ show score)

      -- Time Limit texture 
      timeLimitRender :: Float -> Picture
      timeLimitRender time = 
        if timemodeon game 
          then translate 510 430 $ scale 0.15 0.15 $ color orange $ Text ("Time left: " ++ (show $ round (timelimit game - time)))
           else translate 510 430 $ scale 0.15 0.15 $ color orange $ Text ("Time: " ++ (show $ round (time)))
 
 

