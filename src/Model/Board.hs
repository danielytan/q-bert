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

    -- * Moves
  , up
  , down
  , left
  , right
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
  | BEAN
  deriving (Eq, Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord, Show)

(!) :: Board -> Pos -> Maybe Characters
board ! pos = M.lookup pos board

dim :: Int
dim = 16

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
checkWin v = length (visited v) >= (dim*dim - dim) `div` 2

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

type Direct = Pos -> Pos
up :: Pos -> Pos
up p = p
  { pRow = max (pCol p + 1) (pRow p - 1)
  }

down :: Pos -> Pos
down p = p
  { pRow = min dim (pRow p + 1)
  }

left :: Pos -> Pos
left p = p
  { pCol   = max 1 (pCol p - 1)
  }

right :: Pos -> Pos
right p = p
  { pCol = min (pRow p - 1) (pCol p + 1)
  }

print_direct :: (Pos -> Pos) -> String
print_direct up = "UP"
print_direct down = "DOWN"
print_direct left = "LEFT"
print_direct right = "RIGHT"

boardWinner :: Result a -> Maybe Characters
boardWinner (Win xo) = Just xo
boardWinner _        = Nothing

