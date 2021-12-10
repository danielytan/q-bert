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
header s = printf "Level: %s, Deaths = %s, Points = %s, row = %d, col = %d, gameOver = %s" (show (psWins s + 1)) (show (psDeaths s)) (show (pts)) (pRow p) (pCol p) (show (gameIsOver s))
  where
    p    = psPos s
    pts  = points s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c
  | deathAnimation s > 0 && ((Model.currModel s == MAIN && isCurrPlayer s (r+1) (c-1)) || (Model.currModel s == MAIN' && isCurrPlayer s (r+1) (c+1))) = fillDeathBox white black raw
  | newLevel s > 0 = raw
  | odd (gameIsOver' s) && gameIsOver s && r == 4 && c /= 5 = fillCell red raw
  | r == 3 && c == dim-1 = fillCell (setGoalColor (psWins s)) raw
  | isCurrPlayer s r c = fillCell curr_vcolor raw
  | isVisited s r c && (isCurrSnake s r c || isCurrEnemy s r c ) = fillEnemy goalColor red raw
  | isVisited s r c = fillCell goalColor raw
  | isCurrSnake s r c = fillEnemy unvisitedColor red raw
  | isCurrEnemy s r c = fillEnemy unvisitedColor red raw
  | r >= restrict c = fillCell unvisitedColor raw
  | otherwise    = raw
  where
    --goalColor = setGoalColor (psWins s)
    unvisitedColor = setUnvisitedColor (psWins s)
    vcolor = vsColor (vState s r c) (psWins s) gs
    curr_vcolor = vsColor (nextState (vState s r c) gs) (psWins s) gs
    gs = goalState s
    goalColor = vcolor
    raw = mkCell' s r c

vsColor :: Int -> Int -> Int -> Color
vsColor i w gs
  | i == gs = setGoalColor w
  | i == -1 = (setUnvisitedColor w)
  | i ==  0 = brightGreen
  | i ==  1 = brightCyan
  | i ==  2 = brightBlue
  | otherwise = setGoalColor w

setGoalColor :: Int -> Color
setGoalColor i
  | i <= 1 = blue
  | i <= 3 = magenta
  | i <= 5 = white
  | i <= 7 = yellow
  | otherwise = white

setUnvisitedColor :: Int -> Color
setUnvisitedColor i
  | i <= 1 = yellow
  | i <= 3 = cyan
  | i <= 5 = green
  | i <= 7 = green
  | otherwise = magenta

mid = div (dim + 1) 2
restrict c = mid + abs (c - mid)

fillCell :: Color -> Widget n -> Widget n
fillCell c raw = modifyDefAttr (`withStyle` bold) (modifyDefAttr (`withBackColor` c) raw)

fillEnemy c1 c2 raw = modifyDefAttr (`withStyle` bold) (modifyDefAttr (`withForeColor` c2) (modifyDefAttr (`withBackColor` c1) raw))

fillDeathBox c1 c2 raw = modifyDefAttr (`withStyle` bold) (modifyDefAttr (`withForeColor` c2) (modifyDefAttr (`withBackColor` c1) raw))


mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkXO xoMb)
  where
    --xoMb      = psBoard s ! Pos r c
    xoMb
       | deathAnimation s > 0 && ((Model.currModel s == MAIN && isCurrPlayer s (r+1) (c-1)) || (Model.currModel s == MAIN' && isCurrPlayer s (r+1) (c+1)))  = Just GIB
       | newLevel s > 0 && r == 4 && c == 3 = Just L 
       | newLevel s > 0 && r == 4 && c == 4 = Just E 
       | newLevel s > 0 && r == 4 && c == 5 = Just V 
       | newLevel s > 0 && r == 4 && c == 6 = Just E 
       | newLevel s > 0 && r == 4 && c == 7 = Just L 
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 1 = Just ONE
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 2 = Just TWO
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 3 = Just THREE
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 4 = Just FOUR
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 5 = Just FIVE
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 6 = Just SIX
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 7 = Just SEVEN
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 8 = Just EIGHT
       | newLevel s > 0 && r == 6 && c == 5 && psWins s + 1 == 9 = Just NINE
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 1 = Just G
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 2 = Just A
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 3 = Just M
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 4 = Just E
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 5 = Just ENTER
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 6 = Just O
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 7 = Just V
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 8 = Just E
       | newLevel s == 0 && odd (gameIsOver' s) && gameIsOver s && r == 4 && c == 9 = Just R
       | newLevel s == 0 && r == 2 && c == dim-1 = Just GOAL
       | newLevel s == 0 && isCurrPlayer s r c   = Just (Model.currModel s)
       | newLevel s == 0 && isCurrSnake s r c    = Just (Model.currEnemyModel s)
       | newLevel s == 0 && isCurrEnemy s r c    = Just BEAN
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
mkXO (Just G) = blockG
mkXO (Just A) = blockA
mkXO (Just M) = blockM
mkXO (Just E) = blockE
mkXO (Just O) = blockO
mkXO (Just V) = blockV
mkXO (Just R) = blockR
mkXO (Just ENTER) = blockEnter
mkXO (Just L) = blockL
mkXO (Just ONE) = block1
mkXO (Just TWO) = block2
mkXO (Just THREE) = block3
mkXO (Just FOUR) = block4
mkXO (Just FIVE) = block5
mkXO (Just SIX) = block6
mkXO (Just SEVEN) = block7
mkXO (Just EIGHT) = block8
mkXO (Just NINE) = block9
mkXO (Just GIB) = blockGib


blockB, blockChar, blockSnake, blockBean, blockSquid, blockGoal :: Widget n
blockB = vBox (replicate 5 (str "     "))
blockChar = vBox [ str " ___   "
                 , str "|███|_ "
                 , str "|█████|"
                 , str "|▙ |▙  "]
blockSnake' = vBox [ str ",,▁▁▁"
                  ,  str " ▁▁▁▌"
                  ,  str "▐▁▁▁▁"]
blockChar' = vBox [ str "  ___ "
                 , str " _|███| "
                 , str "|█████| "
                 , str "  ▟| ▟| "]
blockSnake = vBox [ str " ▁▁▁,,"
                  , str " ▌▁▁▁"
                  , str "▁▁▁▁▐"]
blockBean = vBox [ str "______"
              ,   str "|██████|"
              ,   str "|( )( )|"
              ,   str "|██████|"]

blockSquid = vBox [ str " ______"
                ,   str "|(.)(.)|"
                ,   str " |||||| "]

blockGoal = vBox [ str " Color "
               ,   str "  To   "
               ,   str " Fill  "]

blockG = vBox [ str "  ____ "
            ,   str " |   _  "
            ,   str " |____| "]
blockA = vBox [ str "  ____  "
            ,   str " |____| "
            ,   str " |    | "]
blockM = vBox [ str "  _  _"
            ,   str " | \\/ |  "
            ,   str " |    |"]
blockE = vBox [ str "  _____ "
            ,   str " |_____   "
            ,   str " |_____  "]
blockO = vBox [ str "  _____ "
            ,   str " |     | "
            ,   str " |_____| "]
blockV = vBox [ str "        "
            ,   str " \\    /"
            ,   str "  \\  / "
            ,   str "   \\/  "]
blockL = vBox [ str " | "
            ,   str " | "
            ,   str " |____  "]

blockR = vBox [ str " _____ "
            ,   str " |___| "
            ,   str " |\\  "]

block1 = vBox [ str "     |"
            ,   str "     | "
            ,   str "     | "]
block2 = vBox [ str "  ___ "
            ,   str "  ___| "
            ,   str " |___  "]
block3 = vBox [ str "  ___  "
            ,   str "  ___| "
            ,   str "  ___| "]
block4 = vBox [ str "       "
            ,   str " |___| "
            ,   str "     | "]
block5 = vBox [ str "  ___  "
            ,   str " |___  "
            ,   str "  ___| "]
block6 = vBox [ str "  ___  "
            ,   str " |___  "
            ,   str " |___| "]
block7 = vBox [ str "  ___  "
            ,   str "     | "
            ,   str "     | "]
block8 = vBox [ str "  ___  "
            ,   str " |___| "
            ,   str " |___| "]
block9 = vBox [ str "  ___  "
            ,   str " |___| "
            ,   str "  ___| "]

blockGib = vBox [ str "       "
            ,   str " %!&?/?# "
            ,   str "         "]

blockEnter = vBox [ str "   Press  "
            ,   str "   Enter  "
            ,   str "to Restart"]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget