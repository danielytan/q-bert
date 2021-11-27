module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s =<< liftIO (play O s)
  -- T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (play X s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up addVisited   s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down addVisited s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left addVisited s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right addVisited s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> (Vis -> Pos -> Vis) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f1 f2 s = s { 
  psPos = f1 (psPos s),
  psPos2 = f1 (psPos2 s),
  boardVis = if checkWin (f2 (boardVis s) (psPos s)) then Vis [] else f2 (boardVis s) (psPos s) 
}

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


