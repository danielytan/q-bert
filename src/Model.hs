{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import System.Random (Random(..), newStdGen)
import Text.ParserCombinators.ReadP (chainl)
import Control.Concurrent (threadDelay)
import qualified GHC.Base as Task
import Model.Board (Characters)

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data Stream a = a :| Stream a
  deriving (Show)

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psX      :: Player.Player   -- ^ player X info
  , psO      :: Player.Player   -- ^ player O info
  , psScore  :: Score.Score     -- ^ current score
  , psWins   :: Int
  , psDeaths :: Int
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.Characters        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor
  , psPos2   :: Board.Pos       -- ^ second cursor
  , beans    :: [Board.Pos]
  , boardVis  :: Board.Vis
  , psResult :: Board.Result () -- ^ result    
  , lastMove :: Player.Direct
  , nextInteger :: Stream Integer
  , numIters :: Integer
  , currModel :: Characters
  , currEnemyModel :: Characters 
  , gameIsOver :: Bool 
  , gameIsOver' :: Int
  , paused :: Bool
  , newLevel :: Int
  } 

init :: Int -> IO PlayState
init n = do
  rg <- newStdGen
  let lst = fromList (randomRs ((0, 4)::(Integer, Integer)) rg)
  let g = PS { 
    psX      = Player.human
  , psO      = Player.rando
  , psScore  = Score.init n
  , psWins   = 0
  , psDeaths = 0
  , psBoard  = Board.init
  , psTurn   = Board.MAIN
  , psPos    = Board.Pos (div (Board.dim + 1) 2 + 1) (div (Board.dim + 1) 2) 
  , psPos2   = Board.Pos (div (Board.dim + 1) 2 + 3) (div (Board.dim + 1) 2) 
  , beans    = []
  , boardVis  = Board.Vis []
  , psResult = Board.Cont ()
  , lastMove = Player.DOWN
  , nextInteger = lst
  , numIters = 0
  , currModel = Board.MAIN
  , currEnemyModel = Board.SNAKE
  , gameIsOver = False
  , gameIsOver' = 0 
  , paused = False
  , newLevel = 0
  }
  return g
--- >>> randomNum
--- [2,1,2,2,2,1,2,1,1,1]
---
randomNum = do
  g <- newStdGen
  print . take 10 $ (randomRs ((1, 2)::(Integer, Integer)) g)


gameOver :: PlayState -> PlayState
gameOver s = s { 
        gameIsOver = True
      , gameIsOver' = 1 
      , psDeaths = 0
      , psBoard  = Board.init
      , psTurn   = Board.MAIN
      , psPos    = Board.Pos (div (Board.dim + 1) 2 + 1) (div (Board.dim + 1) 2)
      , psPos2   = Board.Pos (div (Board.dim + 1) 2 + 3) (div (Board.dim + 1) 2) 
      , beans    = []
      , boardVis  = Board.Vis []
      , psResult = Board.Cont ()
      , lastMove = Player.DOWN
      , numIters = 0
      , psWins = 0
      , paused = True
    }

checkDeath :: PlayState -> PlayState
checkDeath s 
  | checkLose s && psDeaths s + 1 == 3 = gameOver s
  | checkLose s = s { 
        psDeaths = psDeaths s + 1
      , psBoard  = Board.init
      , psTurn   = Board.MAIN
      , psPos    = Board.Pos (div (Board.dim + 1) 2 + 1) (div (Board.dim + 1) 2)
      , psPos2   = Board.Pos (div (Board.dim + 1) 2 + 3) (div (Board.dim + 1) 2) 
      , beans    = []
      , boardVis  = Board.Vis []
      , psResult = Board.Cont ()
      , lastMove = Player.DOWN
      , numIters = 0
    }
  | otherwise = s



checkLose :: PlayState -> Bool
checkLose s = isCurrEnemy s r c || isCurrSnake s r c
  where
    p = psPos s
    r = Board.pRow p
    c = Board.pCol p 

isCurrPlayer :: PlayState -> Int -> Int -> Bool
isCurrPlayer s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

isCurrEnemy :: PlayState -> Int -> Int -> Bool
isCurrEnemy s r c = foldr (\p-> (||) (Board.pRow p == r && Board.pCol p == c)) False p2s
  where 
    p2s = beans s

isCurrSnake :: PlayState -> Int -> Int -> Bool
isCurrSnake s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos2 s

{- checkVis :: [Pos] -> Pos -> Bool
checkVis bs p
  = foldr
      (\ b -> (||) (pRow p == pRow b && pCol p == pCol b)) False bs

 -}
isVisited :: PlayState -> Int -> Int -> Bool
isVisited s r c = Board.checkVis (Board.visited (boardVis s)) (Board.Pos r c)


next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'})
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

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")