{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import Text.ParserCombinators.ReadP (chainl)

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psX      :: Player.Player   -- ^ player X info
  , psO      :: Player.Player   -- ^ player O info
  , psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.XO        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor
  , psPos2   :: Board.Pos       -- ^ second cursor
  , boardVis  :: Board.Vis
  , psResult :: Board.Result () -- ^ result      
  } 

init :: Int -> PlayState
init n = PS 
  { psX      = Player.human
  , psO      = Player.rando
  , psScore  = Score.init n
  , psBoard  = Board.init
  , psTurn   = Board.X
  , psPos    = Board.Pos 2 1
  , psPos2   = Board.Pos 5 2
  , boardVis  = Board.Vis []
  , psResult = Board.Cont ()
  }

isCurrPlayer :: PlayState -> Int -> Int -> Bool
isCurrPlayer s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

isCurrEnemy :: PlayState -> Int -> Int -> Bool
isCurrEnemy s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos2 s 

isVisited :: PlayState -> Int -> Int -> Bool
isVisited s r c = Board.checkVis (Board.visited (boardVis s)) (Board.Pos r c)


next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'
                                  , psTurn  = Board.flipXO (psTurn s) })
next s res             = nextBoard s res 

nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
nextBoard s res = case res' of
                    Board.Win _ -> Left res' 
                    Board.Draw  -> Left res'
                    _           -> Right s' 
  where 
    sc'  = Score.add (psScore s) (Board.boardWinner res) 
    res' = Score.winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = mempty                -- clear the board
             , psTurn  = Score.startPlayer sc' -- toggle start player
             } 

