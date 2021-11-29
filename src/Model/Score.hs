{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board (Result (..), Characters (..))

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scMax  :: Int  -- ^ total number of boards
  , scX    :: Int  -- ^ points for player X 
  , scO    :: Int  -- ^ points for player O 
  , scD    :: Int  -- ^ drawn games 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0 0 0

add :: Score -> Maybe Characters -> Score
add sc (Just MAIN) = sc { scX = scX sc + 1 }
add sc (Just SNAKE) = sc { scO = scO sc + 1 }
add sc Nothing  = sc { scD = scD sc + 1 }

get :: Score -> Characters -> Int
get Score {..} MAIN = scX 
get Score {..} SNAKE = scO 

currRound :: Score -> Int
currRound Score {..} = scX + scO + scD + 1

startPlayer :: Score -> Characters
startPlayer sc 
  | even (currRound sc) = MAIN
  | otherwise           = SNAKE

winner :: Score -> Result () 
winner sc@Score {..}
  | scX > scO + left = Win MAIN
  | scO > scX + left = Win SNAKE
  | left == 0        = Draw
  | otherwise        = Cont ()
  where 
    left             = 1 + scMax - currRound sc