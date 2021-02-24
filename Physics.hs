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

  -- | Move both paddles depending on the velocity
  movePaddles :: BreakoutGame -- ^ Initial game state
              -> BreakoutGame -- ^ new game with updated paddle position
  movePaddles game = game { player1 = movePaddle paddleStep (player1v game) (player1 game)
                          }

  -- | Update the paddle position
  movePaddle :: Float     -- ^ The step
             -> Float     -- ^ Paddle's velocity
             -> Float     -- ^ The initial player state
             -> Float     -- ^ The new player state with an updated paddle position


  movePaddle step velocity player

        -- ^ No step , no mouvement
        | velocity == 0 = player
        -- ^ Below ceiling, but trying to go down.
        | player >= fromIntegral offset && velocity < 0 =
            player + (step *  velocity)
        -- ^ Under floor, but trying to go up.
        | player <= fromIntegral (-offset) && velocity > 0 =
            player + (step *  velocity)
        -- ^Between the two walls.
        | player > fromIntegral (-offset) && player < fromIntegral offset =
            player + (step *  velocity)
        | otherwise = player

  -- | Ball Universe

  -- | Update the ball position using its current velocity.
  moveBall :: Float     -- ^ The number of seconds since last Update
           -> BreakoutGame  -- ^ The initial game state
           -> BreakoutGame  -- ^ A new game state with an updated ball position


  -- When paused, don't move.
  moveBall _ game@ Game { paused } | paused = game

  -- Moving the ball.
  moveBall seconds game =
    if y' < (-300)
        then game { gameState = Over }
    else game { ballLoc = (x' , y') }
    where
      -- Old locations and velocities
      (x, y) = ballLoc game
      (vx, vy) = ballVel game
      (nvx, nvy) = normalize vx vy $ speed game
      --New locations
      x' = x + nvx * seconds
      y' = y + nvy * seconds


  -- | Detect a collision with a paddle. Upon collisions,
  -- change the velocity of the ball to bounce it off the paddle.
  paddleBounce :: BreakoutGame  -- ^ The initial game state
               -> BreakoutGame  -- ^ A new game state with an updated ball velocity

  paddleBounce game = 
    game { ballVel = (vx', vy') }
    where
        pv = player1v game
        px = player1 game
        (vx, vy) = ballVel game
        (x, y) = ballLoc game
        nx = (x-px)/50
        ny = 1 - (nx^2)
        curSpeed = speed game
        vx' = if paddleCollision game -- && vy < 0
              then
                vx - 2*((x-pv)*nx + y*ny)*nx
                else
                vx

        vy' = if paddleCollision game -- && vy < 0
              then
                  -- Update the velocity
                  vy - 2*((x-pv)*nx + y*ny)*ny
                  else
                  -- Do nothing.Return the old velocity
                  vy
        newSpeed = if pv /= 0
                   then curSpeed + 10
                   else curSpeed


  -- | Detect a collision with one of the side walls. Upon collisions,
  -- update the velocity of the ball to bounce it off the wall.
  wallBounce :: BreakoutGame  -- ^ The initial game state
             -> BreakoutGame  -- ^ A new game state with an updated ball velocity

  wallBounce game = game { ballVel = (vx', vy') }
    where
        -- Radius. Use the same thing as in `render`.

        -- The old Velocities.
        (vx, vy) = ballVel game

        vy' = if wallCollisionTB (ballLoc game) ballRadius
              then
                  -- Update the velocity
                  (-vy)
                  -- -vy
                  else
                  -- Do nothing.Return te old velocity
                  vy
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
    then game { gameState = Winner } else game {ballVel = (vx, vy'), bricks = lst1, brickloc = lst2, score = curScore + 10}
    where
      (vx, vy) = ballVel game
      (ballX, ballY) = ballLoc game
      -- update velocity
      vy' = (-vy)
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
      curScore = score game
    
  -- Helper function for brickBounceTrue to find index where brick was collided
  -- Iterate until reached True then return i  
  getTrueIndex:: Int -> [Bool] -> Int
  getTrueIndex i lst
    | lst!!i == False = getTrueIndex (i+1) lst
    | otherwise = i