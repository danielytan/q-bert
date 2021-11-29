module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Lens.Micro ((&), (.~), (%~), (^.))

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player


-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  AppEvent Tick                   -> Brick.continue (step s)--nextS s =<< liftIO (play O s)
  -- T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (play X s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move UP s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move DOWN s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move LEFT s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move RIGHT s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

step s = move (lastMove s) s

--- >>> 2 `mod` (-3)
--- -1
---
markVist s = s {
  boardVis = if checkWin (addVisited (boardVis s) (psPos s)) then Vis [] else addVisited (boardVis s) (psPos s)
}

updateIter s = if mod newIter 5 ==  0
  then (addEnemy s) {
    numIters = newIter
  }
  else s{
    numIters = newIter
  }
  where newIter = numIters s +1

addEnemy s = s {
  beans = enemy'
}
  where enemy = beans s
        enemy' = enemy ++ [Pos 6 1]
updateEnemy s = s {
  beans = newEnemy,
  nextInteger = newFs
}
  where fs = nextInteger s
        enemy = beans s
        len = length enemy
        nextRandom = nextInt len fs
        newFs = rmLst len fs
        newEnemy = randomMove enemy nextRandom

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
randomMove _ [] = []
randomMove [] _ = []
randomMove (k:ks) (l:ls)
  | l == 0 = up(k):(randomMove ks ls)
  | l == 1 = down(k):(randomMove ks ls)
  | l == 2 = left(k):(randomMove ks ls)
  | otherwise = right(k):(randomMove ks ls)

-------------------------------------------------------------------------------
move :: Direct -> PlayState -> PlayState
-------------------------------------------------------------------------------
move d s
    | d == UP = s''' {
      lastMove = d,
      psPos = up (psPos s)
    }
    | d == DOWN = s''' {
      lastMove = d,

      psPos = down (psPos s)
    }
    | d == LEFT = s''' {
      lastMove = d,
      psPos = left (psPos s)
    }
    | d == RIGHT = s''' {
      lastMove = d,
      psPos = right (psPos s)
    }
    | otherwise = s''
  where s' = markVist s
        s'' = updateIter s'
        s''' = updateEnemy s''


-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = put (psBoard s) xo <$> getPos xo s
  | otherwise      = return Retry

getPos :: XO -> PlayState -> IO Pos
getPos xo s = getStrategy xo s (psPos s) (psBoard s) xo

getStrategy :: XO -> PlayState -> Strategy
getStrategy X s = plStrat (psX s)
getStrategy O s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case Model.next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res })


