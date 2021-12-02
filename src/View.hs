module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board
import Graphics.Vty hiding (dim)


-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vTile [ mkRow s row | row <- [1..dim] ]

header :: PlayState -> String
header s = printf "Wins: %s, Deaths = %s, row = %d, col = %d" (show (psWins s)) (show (psDeaths s)) (pRow p) (pCol p)
  where 
    p    = psPos s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

goalColor = blue

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | r == 3 && c == dim-1 = fillCell goalColor raw
  | isCurrPlayer s r c = fillCell blue raw
  | isVisited s r c && (isCurrSnake s r c || isCurrEnemy s r c ) = fillEnemy blue red raw
  | isVisited s r c = fillCell blue raw
  | isCurrSnake s r c = fillEnemy yellow red raw
  | isCurrEnemy s r c = fillEnemy yellow red raw
  | r >= restrict c = fillCell yellow raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c


mid = div (dim + 1) 2
restrict c = mid + abs (c - mid)

fillCell :: Color -> Widget n -> Widget n
fillCell c raw = modifyDefAttr (`withStyle` bold) (modifyDefAttr (`withBackColor` c) raw)

fillEnemy c1 c2 raw = modifyDefAttr (`withStyle` bold) (modifyDefAttr (`withForeColor` c2) (modifyDefAttr (`withBackColor` c1) raw))


mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkXO xoMb)
  where 
    --xoMb      = psBoard s ! Pos r c
    xoMb 
       | r == 2 && c == dim-1 = Just GOAL
       | isCurrPlayer s r c   = Just (Model.currModel s)
       | isCurrSnake s r c    = Just (Model.currEnemyModel s)
       | isCurrEnemy s r c    = Just BEAN
       | otherwise            = psBoard s ! Pos r c
 

mkXO :: Maybe Characters -> Widget n
mkXO Nothing  = blockB
mkXO (Just MAIN) = blockChar
mkXO (Just SNAKE) = blockSnake
mkXO (Just MAIN') = blockChar'
mkXO (Just SNAKE') = blockSnake'
mkXO (Just BEAN) = blockBean
mkXO (Just SQUID) = blockSquid
mkXO (Just GOAL) = blockGoal

blockB, blockChar, blockSnake, blockBean, blockSquid, blockGoal :: Widget n
blockB = vBox (replicate 5 (str "     "))
blockChar = vBox [ str " ___   "
                 , str "||  |_ "
                 , str "||  __|"
                 , str "|_ |_  "]
blockSnake' = vBox [ str ",,___"
                  , str " ___|"
                  , str "|____"]
blockChar' = vBox [ str "   ___ "
                 , str " _|  || "
                 , str "|__  || "
                 , str "  _| _| "]
blockSnake = vBox [ str "___,,"
                  , str "|___"
                  , str "____|"]
blockBean = vBox [ str "______"
              ,   str "| _  _ |"
              ,   str "|( )( )|"
              ,   str "|______|"]

blockSquid = vBox [ str " ______"
                ,   str "|(.)(.)|"
                ,   str " |||||| "]

blockGoal = vBox [ str " Color "
               ,   str "  To   "
               ,   str " Fill  "]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget