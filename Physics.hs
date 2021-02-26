{-# LANGUAGE NamedFieldPuns #-}

module Physics where

  import GameBoard
  import CollisionDetection

  import Data.Maybe

  -- Normlizes a tuple/vector then multiplies by factor
  normalize :: Float -> Float -> Float -> (Float, Float)
  normalize x y f = (nx, ny)
    where
      norm = sqrt ( x^2 + y^2 )
      nx = x / norm * f
      ny = y / norm * f

  -- Move paddle depending on position and velocity
  movePaddles :: BreakoutGame -> BreakoutGame
  movePaddles game = 
    game { player1 = newx}
      where
        x = player1 game
        vx = player1v game
        newx = if vx == 0 
                then x -- paddle is not moving
               else if x >= fromIntegral offset && vx < 0  -- paddle moving left; position is right of  left wall
                then x + vx
               else if x <= fromIntegral (-offset) && vx > 0  -- paddle moving right; position is left of right wall
                 then x + vx
               else if x > fromIntegral (-offset) && x < fromIntegral offset  -- paddle is between the left and right walls
                 then x + vx
               else x

  -- Given number of seconds passed since last update and initial game state, 
  -- return updated game state with new ball position
  moveBall :: Float -> BreakoutGame -> BreakoutGame
  -- When paused, don't move.
  moveBall _ game@ Game { paused } 
    | paused = game
  -- Moving the ball.
  moveBall seconds game =
    if paused game 
      then game
    else if y' < (-300) 
        then game { gameState = Over, paused = True }    -- if ball is below y=-300, update to game over state
    else game { ballLoc = (x' , y')}
    where
      -- Old locations and velocities, time
      (x, y) = ballLoc game
      (vx, vy) = ballVel game
      (nvx, nvy) = normalize vx vy $ speed game
      --New locations
      x' = x + nvx * seconds
      y' = y + nvy * seconds

  -- Given number of seconds passed since last update and initial game state,
  -- return updated game state with updated timer
  updateTime :: Float -> BreakoutGame -> BreakoutGame
  updateTime _ game@ Game { paused } | paused = game
  updateTime seconds game
    | timemodeon game && time game >= timelimit game = game { gameState = Over, paused = True} -- if over time limit, update to game over state
    | otherwise  = game {time = t'}
      where
        t = time game
        t' = t + seconds

  -- Detect a collision with a paddle. Upon collision,
  -- change the velocity/speed of the ball to bounce it off the paddle.
  -- If no collision, maintain original velocity/speed
  paddleBounce :: BreakoutGame -> BreakoutGame
  paddleBounce game = 
    game { ballVel = (vx', vy'), speed = newSpeed }
    where
        pv = player1v game
        px = player1 game
        (vx, vy) = ballVel game
        (x, y) = ballLoc game
        nx = (x-px)/50
        ny = 1 - (nx^2)
        curSpeed = speed game
        -- 2*((x-pv)*nx + y*ny)*nx is some matrix multiplication to calculate
        -- velocity change
        vx' = if paddleCollision game && vy < 0
                then vx - 2*((x-pv)*nx + y*ny)*nx
              else vx
        vy' = if paddleCollision game && vy < 0
                then vy - 2*((x-pv)*nx + y*ny)*ny
              else vy
        newSpeed = if pv /= 0 && paddleCollision game && vy < 0
                    then curSpeed + 10
                   else curSpeed


  -- Checks if there is collision with one of the side walls. Upon collisions,
  -- update the velocity of the ball to bounce it off the wall.
  -- Otherwise maintain original velocity
  wallBounce :: BreakoutGame -> BreakoutGame 
  wallBounce game = game { ballVel = (vx', vy') }
    where
        -- The old Velocities.
        (vx, vy) = ballVel game
        vy' = if wallCollisionTB (ballLoc game) ballRadius then (-vy) else vy
        vx' = if wallCollisionLR (ballLoc game) ballRadius then (-vx) else vx

  -- Check if ball collides with a wall. If true, update ball velocity to bounce off
  -- and remove the brick by updating relevant brick data (calls helper fn)
  brickBounce :: BreakoutGame -> BreakoutGame
  brickBounce game = gameUpdated
    where
        pair = brickCollision game -- (Bool, [Bool])
        gameUpdated = if fst pair then brickBounceTrue game (snd pair) else game

  -- Helper function for brickBounce
  brickBounceTrue :: BreakoutGame -> [Bool] -> BreakoutGame
  brickBounceTrue game lst = if (all not lst1) 
    then game { gameState = Winner, paused = True, victorytime = completetime, fastesttime = currfastest} else game {ballVel = (vx', vy'), bricks = lst1, brickloc = lst2, score = curScore + 10}
    where
      (vx, vy) = ballVel game
      (ballX, ballY) = ballLoc game
      
      -- call helper to find index where brick was collided
      brickListIndex = getTrueIndex 0 lst
      -- update bricks
      splitlst1 = splitAt brickListIndex $ bricks game
      splitlst1snd = snd splitlst1
      lst1 = fst splitlst1 ++ [False] ++ drop 1 splitlst1snd
      -- update bricksloc
      splitlst2 = splitAt brickListIndex $ brickloc game
      splitlst2snd = snd splitlst2
      lst2 = fst splitlst2 ++ [(-100000,-100000)] ++ drop 1 splitlst2snd -- used -100000 as "equivalent" to INT_MIN in C
      
      -- update velocity
      currBrick = head splitlst2snd
      cond = brickCollisionConditions vx (ballX,ballY) currBrick
      vy' = if cond == 1 || cond == 3 then (-vy)
            else vy -- cond == 2
      vx' = if cond == 1 || cond == 2 then (-vx)
            else vx -- cond == 3

      -- update score
      curScore = score game

      -- find what the time when the level was completed
      completetime = time game
      previousfastest = fastesttime game
      currfastest = 
        if completetime > previousfastest
          then previousfastest
          else completetime

  -- Helper function for brickBounceTrue to find index where brick was collided
  -- Iterate until reached True then return i  
  getTrueIndex:: Int -> [Bool] -> Int
  getTrueIndex i lst
    | lst!!i == False = getTrueIndex (i+1) lst
    | otherwise = i
  
