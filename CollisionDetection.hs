module CollisionDetection where

  import GameBoard

  -- | Given position and radius of the ball, return whether a collision occurred on a player
  -- paddleCollision :: BreakoutGame -- ^ The game
  --                 -> Bool     -- ^ Collision with the paddles?

  paddleCollision game =
    ((deltaXP1 * deltaXP1 + deltaYP1 * deltaYP1) <
      (ballRadius * ballRadius)) 
    where
      -- Ball's center
      (ballX, ballY) = ballLoc game
      -- Player 1's paddle's center
      recXP1 = player1 game
      recYP1 = -200
      -- Player A's paddle's left bottom corner (needed for collision math)
      rectCornerXP1 = recXP1 -paddleWidth / 2
      rectCornerYP1 = recYP1 -paddleHeight / 2

      deltaXP1 = ballX - max rectCornerXP1 (min ballX (rectCornerXP1 + paddleWidth))
      deltaYP1 = ballY - max rectCornerYP1 (min ballY (rectCornerYP1 + paddleHeight))

  -- paddleCollision :: BreakoutGame -> Bool
  -- paddleCollision game =
  --   vy < 0 && x > pleft && x < pright && y > -200 && y < -190
  --     where
  --       (x, y) = ballLoc game
  --       (vx, vy) = ballVel game
  --       px = player1 game
  --       pleft = px - paddleWidth/2
  --       pright = px + paddleWidth/2



  -- Return true if ball collides with top or bottom wall given ball position and radius
  wallCollisionTB :: Position -> Radius -> Bool
  wallCollisionTB (_, y) radius =
      topCollision || bottomCollision 
    where
      topCollision = y + radius >= fromIntegral width / 2
      bottomCollision = y - radius <= -(fromIntegral width / 2)

   -- Return true if ball collides with left or right wall given ball position and radius
  wallCollisionLR :: Position -> Radius -> Bool
  wallCollisionLR (x, _) radius =  rightCollision || leftCollision
    where
      rightCollision = x + radius >= fromIntegral height / 2
      leftCollision = x - radius <= -(fromIntegral height / 2)

  -- Returns a pair (Bool, [Bool]). fst pair indicates if there is a brickCollision in one of the bricks
  -- snd pair is used to find which brick was collided
  brickCollision :: BreakoutGame -> (Bool,[Bool])
  brickCollision game = isTrue
    where
      (ballX, ballY) = ballLoc game
      locationList = brickloc game

      -- apply math formula from paddleCollision to every brick
      locationListCornerLB = map (\(x,y) -> (x-brickwidth/2, y-brickheight/2)) locationList
      locationListDelta = map (\(x,y) -> (ballX - max x (min ballX (x+brickwidth)), ballY - max y (min ballY (y+brickheight)))) locationListCornerLB
      isTrueList = [x*x + y*y < (ballRadius*ballRadius) | (x,y) <- locationListDelta]

      -- if one brick is collided, should return true
      truth = or isTrueList
      isTrue = (truth,isTrueList)
