module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
--import Lens.Micro ((&), (.~), (%~), (^.))

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
import System.Random
import qualified Data.Map as M

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  AppEvent Tick                   -> if deathAnimation s > 0 then 
      Brick.continue (deathAnim s) 
    else 
      if gameIsOver s then 
        Brick.continue (test s) 
      else 
        if newLevel s > 0 then 
          Brick.continue (nextLvl s) 
        else 
          Brick.continue (stepEnemy s)--nextS s =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.continue (newGame s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> if paused s then Brick.continue s else Brick.continue (stepPlayer UP s)
  T.VtyEvent (V.EvKey V.KDown _)  -> if paused s then Brick.continue s else Brick.continue (stepPlayer DOWN s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> if paused s then Brick.continue s else Brick.continue (stepPlayer LEFT s)
  T.VtyEvent (V.EvKey V.KRight _) -> if paused s then Brick.continue s else Brick.continue (stepPlayer RIGHT s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

stepEnemy s = checkDeath (updateEnemy (updateIter s))

stepPlayer dir s = checkWin (checkDeath (move dir s))

deathAnim s = s {
    boardVis  = if (deathAnimation s + 1) `mod` 3 == 0 then Vis M.empty else boardVis s
  , deathAnimation = if deathAnimation s > 0 then (deathAnimation s + 1) `mod` 3 else deathAnimation s
  , psPos    = if (deathAnimation s + 1) `mod` 3 == 0 then Pos (div (dim + 1) 2 + 1) (div (dim + 1) 2) else psPos s
  , psPos2   = if (deathAnimation s + 1) `mod` 3 == 0 then Pos (div (dim + 1) 2 + 3) (div (dim + 1) 2) else psPos2 s
  , paused = if (deathAnimation s + 1) `mod` 3 == 0 && not(gameIsOver s) then False else paused s
}

nextLvl s = s {
  newLevel = if newLevel s > 0 then (newLevel s + 1) `mod` 3 else newLevel s,
  points   = (points s) + 50
}

test s = s {
    gameIsOver =  (gameIsOver' s /= 0) && gameIsOver s,
    gameIsOver' = if gameIsOver' s /= 0 && gameIsOver' s < 5 then gameIsOver' s + 1 else gameIsOver' s
  }

newGame s = if gameIsOver s then s {
  gameIsOver = False,
  gameIsOver' = 0,
  paused = False
}
  else 
    s
--- >>> 2 `mod` (-3)
--- -1
---
markVist s = s {
    boardVis = addVisited (psWins s) visitedTiles p gs
  , points   = if not (checkVis (visited visitedTiles) p) then pts + 1 else pts
}
  where
    p = psPos s
    pts = points s
    visitedTiles = boardVis s
    gs = goalState s

addPoints s i = s {
  points = (points s) + i
}

updateIter s = if psWins s > 1 then 
  if mod newIter 3 == 0 then 
    (addEnemy s (numIters s)) {
      numIters = newIter
    }
  else s {
    numIters = newIter
  }
else
  s
    where newIter = numIters s + 1


--- >>> mod 4 3
--- 1
---

toRow :: [Int]
toRow = [8,7,6,5,4,5,6,7,8]

addEnemy :: PlayState -> Integer -> PlayState
addEnemy s n = s {
  beans = enemy',
  nextInteger2 = fs
}
  where enemy = beans s
        --enemy' = let r = mod n (toInteger (dim)) in enemy ++ [Pos (fromInteger r) ((fromInteger r) - 1)]
        f:|fs = nextInteger2 s
        f' = fromInteger f
        enemy' = enemy ++ [Pos (toRow !! (f' - 1)) f']

updateEnemy s = s {
  beans = newEnemy,
  psPos2 = newSnake,
  nextInteger = newFs',
  currEnemyModel = orientSnake s snakeMove
}
  where fs = nextInteger s
        enemy = beans s
        len = length enemy
        nextRandom = nextInt len fs
        newFs = rmLst len fs
        newEnemy = enforceValidPos $ moveDown enemy nextRandom
        nextRandom' = nextInt 1 newFs
        newFs' = rmLst 1 newFs
        snakeMove = head nextRandom'
        newSnake = randomMove' (psPos2 s) snakeMove

orientSnake s l
  | l == 2 = SNAKE'
  | l == 3 = SNAKE
  | otherwise = currEnemyModel s

nextInt 0 _ = []
nextInt num (f:|fs) = f:nextInt (num-1) fs

rmLst 0 fs = fs
rmLst num (_:|fs) = rmLst (num-1) fs

--- >>> rmLst 2 [1,23,4]
--- [4]
---
k = [Pos 1 1, Pos 6 1]
l = [0, 1]
--- >>> length k
--- 2
---

--- >>> randomMove k l
--- [Pos {pRow = 2, pCol = 1},Pos {pRow = 6, pCol = 2}]
---
moveDown _ [] = []
moveDown [] _ = []
moveDown (k:ks) (l:ls) = down'(k):(moveDown ks ls)

randomMove' :: Pos -> Integer -> Pos
randomMove' k l
  | l == 0 = up (k)
  | l == 1 = down (k)
  | l == 2 = left (k)
  | otherwise = right (k)

-------------------------------------------------------------------------------
move :: Direct -> PlayState -> PlayState
-------------------------------------------------------------------------------
move d s
    | d == UP = s' {
      lastMove = d,
      psPos = up (psPos s)
    }
    | d == DOWN = s' {
      lastMove = d,

      psPos = down (psPos s)
    }
    | d == LEFT = s' {
      lastMove = d,
      psPos = left (psPos s),
      currModel = MAIN'
    }
    | d == RIGHT = s' {
      lastMove = d,
      psPos = right (psPos s),
      currModel = MAIN
    }
    | otherwise = s'
  where s' = markVist s


-------------------------------------------------------------------------------
play :: Characters -> PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = put (psBoard s) xo <$> getPos xo s
  | otherwise      = return Retry

getPos :: Characters -> PlayState -> IO Pos
getPos xo s = getStrategy xo s (psPos s) (psBoard s) xo

getStrategy :: Characters -> PlayState -> Strategy
getStrategy MAIN s = plStrat (psX s)
getStrategy SNAKE s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case Model.next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res })


