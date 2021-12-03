{-# LANGUAGE DeriveFunctor #-}
module Model.Board
  ( -- * Types
    Board
  , Characters (..)
  , Pos (..)
  , Vis (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , positions
  , emptyPositions
  , boardWinner
  , checkVis
  , addVisited
  , checkWin
  , enforceValidPos

    -- * Moves
  , up
  , down
  , left
  , right
  , down'
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos Characters

data Characters
  = MAIN
  | SNAKE 
  | MAIN'
  | SNAKE'
  | BEAN
  | SQUID
  | GOAL
  | G
  | A
  | M
  | E
  | O
  | V
  | R
  | ENTER
  | L
  | ONE
  | TWO
  | THREE
  | FOUR
  | FIVE
  | SIX
  | SEVEN
  | EIGHT
  | NINE
  deriving (Eq, Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord, Show)

(!) :: Board -> Pos -> Maybe Characters
board ! pos = M.lookup pos board

-- MUST BE ODD NUMBER GREATER THAN 4
dim :: Int
dim = 9

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ]


-- >>> visited
-- [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
newtype Vis = Vis
 {
   visited :: [Pos]
 }
 deriving (Eq, Ord)

checkWin :: Vis -> Bool 
checkWin v = length (visited v) >= (dim - 4) * (dim - 4)

addVisited :: Vis -> Pos -> Vis
addVisited v p = v
  { visited = alterVis (visited v) p}


alterVis bs p = if not (checkVis bs p) then p:bs else bs

checkVis :: [Pos] -> Pos -> Bool
checkVis bs p
  = foldr
      (\ b -> (||) (pRow p == pRow b && pCol p == pCol b)) False bs

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

init :: Board
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------

data Result a
  = Draw
  | Win Characters
  | Retry
  | Cont a
  deriving (Eq, Functor, Show)

put :: Board -> Characters -> Pos -> Result Board
put board xo pos = case M.lookup pos board of
  Just _  -> Retry
  Nothing -> result (M.insert pos xo board)

result :: Board -> Result Board
result b
  | isFull b  = Draw
  | wins b MAIN  = Win  MAIN
  | wins b SNAKE  = Win  SNAKE
  | otherwise = Cont b

wins :: Board -> Characters -> Bool
wins b xo = or [ winsPoss b xo ps | ps <- winPositions ]

winsPoss :: Board -> Characters -> [Pos] -> Bool
winsPoss b xo ps = and [ b!p == Just xo | p <- ps ]

winPositions :: [[Pos]]
winPositions = rows ++ cols ++ diags

rows, cols, diags :: [[Pos]]
rows  = [[Pos r c | c <- [1..dim]] | r <- [1..dim]]
cols  = [[Pos r c | r <- [1..dim]] | c <- [1..dim]]
diags = [[Pos i i | i <- [1..dim]], [Pos i (dim+1-i) | i <- [1..dim]]]

isFull :: Board -> Bool
isFull b = M.size b == dim * dim

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Pos -> Pos
up p = p
  { pRow = let mid = div (dim + 1) 2 in max (mid + abs (pCol p - mid)) (pRow p - 1)
  }

down :: Pos -> Pos
down p = p
  { pRow = min dim (pRow p + 1)
  }

down' :: Pos -> Pos
down' p = p
  {
    pRow = pRow p + 1
  }

enforceValidPos :: [Pos] -> [Pos]
enforceValidPos xs = filter (\p -> (pRow p <= dim) ) xs

left :: Pos -> Pos
left p = p
  { pCol   = let mid = div (dim + 1) 2 in max (pRow p - 2 * abs(pRow p - mid)) (pCol p - 1)
  }

right :: Pos -> Pos
right p = p
  { pCol = min (pRow p) (pCol p + 1)
  }

boardWinner :: Result a -> Maybe Characters
boardWinner (Win xo) = Just xo
boardWinner _        = Nothing

