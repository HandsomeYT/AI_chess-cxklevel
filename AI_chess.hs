module Main where
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Text.Printf
import qualified Data.List as D
import qualified Control.Arrow
import qualified Data.Text as T
import Data.Char
import qualified Data.List.Split as S
import Control.Monad
import System.IO
import System.Directory
import Control.Concurrent
--import Myjson



data Tree a = Branch [Tree a] | Leaf a

data Qi = X | O

qToChar :: Qi -> Char
qToChar X = 'X'
qToChar O = 'O'

qToString :: Qi -> String
qToString q = [qToChar q]

instance Eq Qi where
  a == b = qToChar a == qToChar b

instance Show Qi where
  show = qToString

instance Read Qi where
  readsPrec _ input--readsPrec解析生成的字符串 showsPrec，并传递以其showsPrec开头的值 
    | head input == 'X' = [(X, tail input)]
    | head input == 'O' = [(O, tail input)]
    | otherwise = []

data Field = Field Int Int 

instance Show Field where
  show (Field x y) = show (x-1) ++ " " ++ show (y-1)

instance Eq Field where
  (Field x1 y1) == (Field x2 y2) = (x1 == x2)  && (y1 == y2)

instance Ord Field where
  compare (Field x1 y1) (Field x2 y2)
    | y1 > y2 = GT
    | y1 < y2 = LT
    | x1 > x2 = GT
    | x1 < x2 = LT
    | otherwise = EQ

data Board = Board (Map.Map Field Qi) Int Int

data Score = Score Double | Occupied

instance Eq Score where
  (Score x) == (Score y) = x == y
  _ == _ = False

instance Ord Score where
  compare (Score x) (Score y) = compare y x
  compare Occupied (Score _) = GT
  compare (Score _) Occupied = LT
  compare Occupied Occupied = EQ

instance Show Score where
  show = scoreToString

data ScoreBoard = ScoreBoard [[Score]] Int Int

--打印棋盘

{-boardfieldToChar :: Maybe Qi -> String--Maybe 类型，在计算过程错误的时候，可以让最终计算结果为Nothing。
boardfieldToChar (Just q) = [' ', qToChar q, ' ']
boardfieldToChar Nothing = " - "

getBoardLine :: Board -> Int -> String
getBoardLine (Board boardMap x _) line = concat ([boardfieldToChar(Map.lookup (Field wdth line) boardMap)
                                                  | wdth <- [1..x-1]] ++ [boardfieldToChar(Map.lookup (Field x line) boardMap)])

addNewLineAndNumber :: Int -> String -> String
addNewLineAndNumber i str = Text.Printf.printf "%2d " i ++ str ++ ['\n']

boardToString :: Board -> String
boardToString (Board boardMap x y) = "    1  2  3  4  5  6  7  8  9  10 11 12 13 14 15\n" ++
                                     concat [addNewLineAndNumber line (getBoardLine (Board boardMap x y) line) | line <- [1..y]]
-}
scoreToString :: Score -> String
scoreToString (Score score) = Text.Printf.printf "%2.2f" score
scoreToString Occupied = "XXX"

--偷来的极大极小值算法(逃

getMoveTree :: Int -> Int -> Board -> Qi -> Qi -> Tree Double
getMoveTree 0 _ board _ targetPlayer = Leaf (getBoardScore board targetPlayer)
getMoveTree depth branching board player targetPlayer = Branch [getMoveTree (depth-1) branching branchingBoard (negateQi player) targetPlayer | branchingBoard <- createBranches branching board player]

createBranches :: Int -> Board -> Qi -> [Board]
createBranches branching board player = map (\(_, (x, y)) -> setQi board (Field x y) player) (take branching (listBestMoves (getScoreBoard board player)))

getMaxScoreField :: Board -> Qi -> [(Double, Field)] -> Field
getMaxScoreField board player list = (\[(_, x)] -> x) $ if (\[(x,_)] -> x /= 0) $ take 1 $ D.sortBy (\(x, _) (y, _) -> y `compare` x) list then
                                           take 1 $ D.sortBy (\(x, _) (y, _) -> y `compare` x) list else
                                           (\(Score val, (xpos, ypos)) -> [(val, Field xpos ypos)]) $ head $ listBestMoves $ getScoreBoard board player

zipMaxWithFields :: Int -> Int -> Board -> Qi -> [(Double, Field)]
zipMaxWithFields depth branching board player = zipWith (\x y -> (x, y))
                                                (leafBranchToList $ getMaxWin $ getMoveTree depth branching board player player)
                                                (movesToFieldsList $ take branching $ listBestMoves $ getScoreBoard board player)

leafBranchToList :: Tree Double -> [Double]
leafBranchToList (Branch list@(Leaf _:_)) = map (\(Leaf val) -> val) list
leafBranchToList _ = []

movesToFieldsList :: [(t, (Int,Int))] -> [Field]
movesToFieldsList = map (\(_, (x, y)) -> Field x y)

getMaxWin :: Tree Double -> Tree Double
getMaxWin tree@(Branch (Branch _:_)) = getMaxWin (minToMax tree)
getMaxWin tree = tree

maxToMin :: Tree Double -> Tree Double
maxToMin (Branch list@(Branch _:_)) = Branch [minToMax elem | elem <- list]
maxToMin (Branch list) = Leaf (maximum (map (\(Leaf x) -> x) list))
maxToMin (Leaf x) = Leaf x

minToMax :: Tree Double -> Tree Double
minToMax (Branch list@(Branch _:_)) = Branch [maxToMin elem | elem <- list]
minToMax (Branch list) = Leaf (minimum (map (\(Leaf x) -> x) list))
minToMax (Leaf x) = Leaf x



isFieldOccupied :: Maybe Qi -> Bool
isFieldOccupied (Just _) = True
isFieldOccupied Nothing = False
--下棋
setQi :: Board -> Field -> Qi -> Board
setQi (Board boardMap x y) field q = Board (Map.insert field q boardMap) x y

negateQi :: Qi -> Qi
negateQi X = O
negateQi O = X

--评分

calculateFieldScore :: Field -> Board -> Qi -> Double
calculateFieldScore field board q = sum [horizontalMatch field board q,
                                            verticalMatch field board q,
                                            closeToCenterPoints field board,
                                            closeToOtherQiPoints field board,
                                            searchWin field board (negateQi q),
                                            threatSearch field board q,
                                            searchWin field board q,
                                            ascSlopeMatch field board q,
                                            descSlopeMatch field board q]

closeToCenterPoints :: Field -> Board -> Double
closeToCenterPoints (Field xf yf) (Board boardMap x y) = 0.5 * (sqrt((fromIntegral (x+1) / 2) ^ 2 + (fromIntegral (y+1) / 2) ^ 2) -
                                                         sqrt(((fromIntegral (x+1) / 2) - fromIntegral xf) ^ 2 +
                                                              ((fromIntegral (y+1) / 2) - fromIntegral yf) ^ 2))

closeToOtherQiPoints :: Field -> Board -> Double
closeToOtherQiPoints (Field xf yf) (Board boardMap x y) = sum (map (\x -> if x then 5 else 0) [isFieldOccupied (Map.lookup (Field closex closey) boardMap) |
                                                                        closex <- [xf-1..xf+1], closey <- [yf-1..yf+1]])

getFieldOccupationScore :: Qi -> Field -> Board -> Double
getFieldOccupationScore player (Field xf yf) (Board boardMap x y)
  | Data.Maybe.isNothing (Map.lookup (Field xf yf) boardMap) = 0
  | Map.lookup (Field xf yf) boardMap == Just player = 15
  | otherwise = -1

horizontalLeft :: Field -> Board -> Qi -> Int -> Double
horizontalLeft (Field xf yf) (Board boardMap x y) player pos
  | getFieldOccupationScore player (Field pos yf) (Board boardMap x y) == -1 = 0
  | xf-pos > 4 = 0
  | otherwise = horizontalLeft (Field xf yf) (Board boardMap x y) player (pos-1) +
                  getFieldOccupationScore player (Field pos yf) (Board boardMap x y)

horizontalRight :: Field -> Board -> Qi -> Int -> Double
horizontalRight (Field xf yf) (Board boardMap x y) player pos
  | getFieldOccupationScore player (Field pos yf) (Board boardMap x y) == -1 = 0
  | pos-xf > 4 = 0
  | otherwise = horizontalRight (Field xf yf) (Board boardMap x y) player (pos+1) +
                  getFieldOccupationScore player (Field pos yf) (Board boardMap x y)

horizontalMatch :: Field -> Board -> Qi -> Double
horizontalMatch (Field xf yf) (Board boardMap x y) q = horizontalLeft (Field xf yf) (Board boardMap x y) q (xf-1) +
                                                     horizontalRight (Field xf yf) (Board boardMap x y) q (xf+1)

verticalDown :: Field -> Board -> Qi -> Int -> Double
verticalDown (Field xf yf) (Board boardMap x y) player pos
 | getFieldOccupationScore player (Field xf pos) (Board boardMap x y) == -1 = 0
 | yf-pos > 4 = 0
 | otherwise = verticalDown (Field xf yf) (Board boardMap x y) player (pos-1) +
                 getFieldOccupationScore player (Field xf pos) (Board boardMap x y)

verticalUp :: Field -> Board -> Qi -> Int -> Double
verticalUp (Field xf yf) (Board boardMap x y) player pos
 | getFieldOccupationScore player (Field xf pos) (Board boardMap x y) == -1 = 0
 | pos-yf > 4 = 0
 | otherwise = verticalUp (Field xf yf) (Board boardMap x y) player (pos+1) +
                 getFieldOccupationScore player (Field xf pos) (Board boardMap x y)

verticalMatch :: Field -> Board -> Qi -> Double
verticalMatch (Field xf yf) (Board boardMap x y) q = verticalDown (Field xf yf) (Board boardMap x y) q (yf-1) +
                                                    verticalUp (Field xf yf) (Board boardMap x y) q (yf+1)

slopeUpRight :: Field -> Board -> Qi -> Int -> Int -> Double
slopeUpRight (Field xf yf) (Board boardMap x y) player posx posy
 | getFieldOccupationScore player (Field posx posy) (Board boardMap x y) == -1 = 0
 | posy-yf > 4 = 0
 | otherwise = slopeUpRight (Field xf yf) (Board boardMap x y) player (posx+1) (posy+1) +
                 getFieldOccupationScore player (Field posx posy) (Board boardMap x y)

slopeDownLeft :: Field -> Board -> Qi -> Int -> Int -> Double
slopeDownLeft (Field xf yf) (Board boardMap x y) player posx posy
  | getFieldOccupationScore player (Field posx posy) (Board boardMap x y) == -1 = 0
  | yf-posy > 4 = 0
  | otherwise = slopeDownLeft (Field xf yf) (Board boardMap x y) player (posx-1) (posy-1) +
                  getFieldOccupationScore player (Field posx posy) (Board boardMap x y)

ascSlopeMatch :: Field -> Board -> Qi -> Double
ascSlopeMatch (Field xf yf) (Board boardMap x y) q = slopeUpRight (Field xf yf) (Board boardMap x y) q (xf+1) (yf+1) +
                                                    slopeDownLeft (Field xf yf) (Board boardMap x y) q (xf-1) (yf-1)

slopeDownRight :: Field -> Board -> Qi -> Int -> Int -> Double
slopeDownRight (Field xf yf) (Board boardMap x y) player posx posy
  | getFieldOccupationScore player (Field posx posy) (Board boardMap x y) == -1 = 0
  | yf-posy > 4 = 0
  | otherwise = slopeDownRight (Field xf yf) (Board boardMap x y) player (posx+1) (posy-1) +
                 getFieldOccupationScore player (Field posx posy) (Board boardMap x y)

slopeUpLeft :: Field -> Board -> Qi -> Int -> Int -> Double
slopeUpLeft (Field xf yf) (Board boardMap x y) player posx posy
  | getFieldOccupationScore player (Field posx posy) (Board boardMap x y) == -1 = 0
  | posy-yf > 4 = 0
  | otherwise = slopeUpLeft (Field xf yf) (Board boardMap x y) player (posx-1) (posy+1) +
                  getFieldOccupationScore player (Field posx posy) (Board boardMap x y)

descSlopeMatch :: Field -> Board -> Qi -> Double
descSlopeMatch (Field xf yf) (Board boardMap x y) q = slopeDownRight (Field xf yf) (Board boardMap x y) q (xf+1) (yf-1) +
                                                    slopeUpLeft (Field xf yf) (Board boardMap x y) q (xf-1) (yf+1)

getFieldScore :: Field -> Board -> Qi -> Score
getFieldScore (Field xf yf) (Board boardMap x y) q
  | isFieldOccupied (Map.lookup (Field xf yf) boardMap) = Occupied
  | otherwise = Score (calculateFieldScore (Field xf yf) (Board boardMap x y) q)

getScoreBoard :: Board -> Qi -> ScoreBoard
getScoreBoard (Board boardMap x y) q = ScoreBoard [[getFieldScore (Field xf yf) (Board boardMap x y) q | xf <- [1..x]] | yf <-[1..y]] x y

listBestMoves :: ScoreBoard -> [(Score, (Int, Int))]
listBestMoves (ScoreBoard scoreList _ _) = D.sortBy (\(x, _) (y, _) -> x `compare` y)
                                  (zipWith (\x y -> (x, y)) (concat scoreList) [(x, y) | y <- [1..15], x <- [1..15]])
--向上搜寻
searchWinUp :: Field -> Board -> Qi -> Double
searchWinUp (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field xf (yf+1)) boardMap == Just player = 1 + searchWinUp (Field xf (yf+1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field xf (yf+1)) boardMap) = 0.6
  | otherwise = 0
--向下
searchWinDown :: Field -> Board -> Qi -> Double
searchWinDown (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field xf (yf-1)) boardMap == Just player = 1 + searchWinDown (Field xf (yf-1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field xf (yf-1)) boardMap) = 0.6
  | otherwise = 0
--向左
searchWinLeft :: Field -> Board -> Qi -> Double
searchWinLeft (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf-1) yf) boardMap == Just player = 1 + searchWinLeft (Field (xf-1) yf) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf-1) yf) boardMap) = 0.6
  | otherwise = 0
--右
searchWinRight :: Field -> Board -> Qi -> Double
searchWinRight (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf+1) yf) boardMap == Just player = 1 + searchWinRight (Field (xf+1) yf) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf+1) yf) boardMap) = 0.6
  | otherwise = 0
--左上
searchWinUpLeft :: Field -> Board -> Qi -> Double
searchWinUpLeft (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf-1) (yf+1)) boardMap == Just player = 1 + searchWinUpLeft (Field (xf-1) (yf+1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf-1) (yf+1)) boardMap) = 0.6
  | otherwise = 0
--右下
searchWinDownRight :: Field -> Board -> Qi -> Double
searchWinDownRight (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf+1) (yf-1)) boardMap == Just player = 1 + searchWinDownRight (Field (xf+1) (yf-1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf+1) (yf-1)) boardMap) = 0.6
  | otherwise = 0
--右上
searchWinUpRight :: Field -> Board -> Qi -> Double
searchWinUpRight (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf+1) (yf+1)) boardMap == Just player = 1 + searchWinUpRight (Field (xf+1) (yf+1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf+1) (yf+1)) boardMap) = 0.6
  | otherwise = 0
--左下
searchWinDownLeft :: Field -> Board -> Qi -> Double
searchWinDownLeft (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf-1) (yf-1)) boardMap == Just player = 1 + searchWinDownLeft (Field (xf-1) (yf-1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf-1) (yf-1)) boardMap) = 0.6
  | otherwise = 0
--啊啊好烦啊，明天再写
searchWin :: Field -> Board -> Qi -> Double
searchWin field board player
  | closeToOtherQiPoints field board == 0 = 0
  | searchWinUp field board player + searchWinDown field board player > 4 = 1000
  | searchWinLeft field board player + searchWinRight field board player > 4 = 1000
  | searchWinUpLeft field board player + searchWinDownRight field board player > 4 = 1000
  | searchWinUpRight field board player + searchWinDownLeft field board player > 4 = 1000
  | otherwise = 0

threatSearch :: Field -> Board -> Qi -> Double
threatSearch field board player
  | closeToOtherQiPoints field board == 0 = 0
  | searchWinUp field board (negateQi player) + searchWinDown field board (negateQi player) > 3 = 200
  | searchWinLeft field board (negateQi player) + searchWinRight field board (negateQi player) > 3 = 200
  | searchWinUpLeft field board (negateQi player) + searchWinDownRight field board (negateQi player) > 3 = 200
  | searchWinUpRight field board (negateQi player) + searchWinDownLeft field board (negateQi player) > 3 = 200
  | otherwise = 0

winCheck :: Board -> Qi -> Bool
winCheck (Board boardMap _ _) player = any (fieldWinCheck boardMap) (filter (\(x, y) -> y == player) (Map.toList boardMap))

fieldWinCheck :: Map.Map Field Qi -> (Field, Qi) -> Bool
fieldWinCheck boardMap (field, q) = verticalWinCheck boardMap (field, q) > 4 ||
                                       horizontalWinCheck boardMap (field, q) > 4 ||
                                       downwardsWinCheck boardMap (field, q) > 4 ||
                                       upwardsWinCheck boardMap (field, q) > 4

verticalWinCheck :: Map.Map Field Qi -> (Field, Qi) -> Int
verticalWinCheck boardMap (Field xf yf, q)
  | Map.lookup (Field xf (yf+1)) boardMap == Just q = 1 + verticalWinCheck boardMap (Field xf (yf+1), q)
  | otherwise = 1

horizontalWinCheck :: Map.Map Field Qi -> (Field, Qi) -> Int
horizontalWinCheck boardMap (Field xf yf, q)
  | Map.lookup (Field (xf+1) yf) boardMap == Just q = 1 + horizontalWinCheck boardMap (Field (xf+1) yf, q)
  | otherwise = 1

downwardsWinCheck :: Map.Map Field Qi -> (Field, Qi) -> Int
downwardsWinCheck boardMap (Field xf yf, q)
  | Map.lookup (Field (xf+1) (yf-1)) boardMap == Just q = 1 + downwardsWinCheck boardMap (Field (xf+1) (yf-1), q)
  | otherwise = 1

upwardsWinCheck :: Map.Map Field Qi -> (Field, Qi) -> Int
upwardsWinCheck boardMap (Field xf yf, q)
  | Map.lookup (Field (xf+1) (yf+1)) boardMap == Just q = 1 + upwardsWinCheck boardMap (Field (xf+1) (yf+1), q)
  | otherwise = 1

getBoardScore :: Board -> Qi -> Double
getBoardScore board player = if winCheck board (negateQi player) then (-10000) else 0 +
                             if winCheck board player then 10000 else 0 +
                             sum [costFunc (Field x y) board player | y <- [1..15], x <- [1..15]]

costFunc :: Field -> Board -> Qi -> Double
costFunc field board@(Board boardMap _ _) player
    | isFieldOccupied (Map.lookup field boardMap) = 0
    | otherwise = threatSearch field board (negateQi player) +
                  searchWin field board player -
                  (searchWin field board (negateQi player) * 10)

--van 游戏

makeMove :: Board -> Qi -> Board
makeMove board q = setQi board (getMaxScoreField board q $ zipMaxWithFields 4 3 board q) q

enemy :: [[Int]] -> Int -> Board -> IO() 
enemy list a board
  | length list == 2 = do 
    ai_one 2 (setQi (setQi board (Field 8 8 ) X) (Field (((list !! 1) !! 1)+1) (((list !! 1) !! 2)+1)) O)
  | a == 0 = do
    ai_one a emptyBoard
  | a == 1 = do 
    ai_one a (setQi board (Field (((list !! 0) !! 1)+1) (((list !! 0) !! 2)+1)) X)
  | otherwise = do
    if (mod a 2 == 0) then enemy list (a-1) (setQi board (Field (((list !! (a-1)) !! 1)+1) (((list !! (a-1)) !! 2)+1)) O) else enemy list (a-1) (setQi board (Field (((list !! (a-1)) !! 1)+1) (((list !! (a-1)) !! 2)+1)) X)

enemy2 :: [[Int]] -> Int -> Board -> IO() 
enemy2 list a board
  | a == 1 = do 
    ai_two a (setQi board (Field (((list !! 0) !! 1)+1) (((list !! 0) !! 2)+1)) X)
  | otherwise = do
    if (mod a 2 == 0) then enemy2 list (a-1) (setQi board (Field (((list !! (a-1)) !! 1)+1) (((list !! (a-1)) !! 2)+1)) O) else enemy2 list (a-1) (setQi board (Field (((list !! (a-1)) !! 1)+1) (((list !! (a-1)) !! 2)+1)) X)

ai_one :: Int -> Board -> IO()
ai_one a board
  | a == 0 = do
    writeFile "E:\\Judge\\coor_1.txt" (show (Field 8 8))
    copyFile "E:\\Judge\\coor_1.txt" "E:\\Judge\\folder_1\\coor_1.txt"
  | a == 2 = do
    writeFile "E:\\Judge\\coor_1.txt" (show (Field 5 5))
    copyFile "E:\\Judge\\coor_1.txt" "E:\\Judge\\folder_1\\coor_1.txt"
    --putStrLn (boardToString (setQi board (Field  5 8 ) X))
  | otherwise = do
    writeFile "E:\\Judge\\coor_1.txt" (show (getMaxScoreField board X $ zipMaxWithFields 6 4 board X))
    copyFile "E:\\Judge\\coor_1.txt" "E:\\Judge\\folder_1\\coor_1.txt"
    --putStrLn (boardToString (makeMove board X))

ai_two :: Int -> Board -> IO()
ai_two a board = do
    writeFile "E:\\Judge\\coor_2.txt" (show (getMaxScoreField board O $ zipMaxWithFields 6 4 board O))
    copyFile "E:\\Judge\\coor_2.txt" "E:\\Judge\\folder_2\\coor_2.txt"
    --putStrLn (boardToString (makeMove board O))

emptyBoard = Board Map.empty 15 15

black :: IO()
black = do 
  --threadDelay(300000)
  fileExist1 <- doesFileExist "E:/Judge/folder_1/board_1.json" 
  if fileExist1 then do
    list <- groupsOf3
    enemy list (length list) emptyBoard
    removeFile "E:/Judge/folder_1/board_1.json"
    black
    else black

white :: IO()
white = do 
  --threadDelay(300000)
  fileExist2 <- doesFileExist "E:/Judge/folder_2/board_2.json"
  if fileExist2 then do 
    list2 <- groupsOf3two
    enemy2 list2 (length list2) emptyBoard
    removeFile "E:/Judge/folder_2/board_2.json"
    white
    else white

main :: IO()
main = do
  fileExist1 <- doesFileExist "E:/Judge/folder_1/board_1.json" 
  if fileExist1 then do
    black
    else white
  main
--处理json
groupsOf3 :: IO[[Int]]
groupsOf3 = do
  list <- toInte 
  let list2 = group1 list
  let list3 = [a|a<-(group2 list2),a/=[]]
  return list3

groupsOf3two :: IO[[Int]]
groupsOf3two = do
  list <- toInte2 
  let list2 = group1 list
  let list3 = [a|a<-(group2 list2),a/=[]]
  return list3 
  
group1 :: [Int] -> [[Int]]
group1 [] = [[]]
group1 list = [take 3 list] ++ [drop 3 list]

group2 :: [[Int]] -> [[Int]]
group2 [[]] = [[]]
group2 list = [head list] ++ group2(group1 (last list))

toInte :: IO[Int]
toInte = do
  --filename <- getLine
  json <- readFile "E:/Judge/folder_1/board_1.json"
  threadDelay(300000)
  let x1 = concat (S.splitOn "\"" json)
  let x2 = concat (S.splitOn "\n" x1)
  let x3 = concat (S.splitOn "[" x2)
  let x4 = concat (S.splitOn "{" x3)
  --let x5 = concat (S.splitOn "," x4)
  --let x6 =concat (S.splitOn ":" x5)
  let x7 =concat (S.splitOn "}" x4)
  let x8 =concat (S.splitOn "]" x7)
  let list = S.endByOneOf " :," x8
  let list2 = [toint s|s<-list,s/=""]
  return list2
  {-let y = [digitToInt a|a <- x,isDigit a]
  return y-}
toInte2 :: IO[Int]
toInte2 = do
  --filename <- getLine
  json <- readFile "E:/Judge/folder_2/board_2.json"
  threadDelay(300000)
  let x1 = concat (S.splitOn "\"" json)
  let x2 = concat (S.splitOn "\n" x1)
  let x3 = concat (S.splitOn "[" x2)
  let x4 = concat (S.splitOn "{" x3)
  --let x5 = concat (S.splitOn "," x4)
  --let x6 =concat (S.splitOn ":" x5)
  let x7 =concat (S.splitOn "}" x4)
  let x8 =concat (S.splitOn "]" x7)
  let list = S.endByOneOf " :," x8
  let list2 = [toint s|s<-list,s/=""]
  return list2
toint :: String->Int
toint' :: Char->Int
toint' s
    |s=='1' =1
    |s=='2' =2
    |s=='3' =3
    |s=='4' =4
    |s=='5' =5
    |s=='6' =6
    |s=='7' =7
    |s=='8' =8
    |s=='9' =9
    |s=='0' =0
    |otherwise = 0
toint (s:xs)=if null xs then toint' s else (toint xs)+(toint' s)*10
