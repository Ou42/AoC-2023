module Main where

import Data.Bifunctor ( Bifunctor(bimap) )
import qualified Data.Map as M
import Data.Set (powerSet)

{-
    Day 02

      Part A
      - a small bag and some cubes which are either red, green, or blue.
      - Each time you play this game, he will hide a secret number of cubes
        of each color in the bag, and your goal is to figure out ...
      - once a bag has been loaded with cubes, the Elf will reach into the bag,
        grab a handful of random cubes, show them to you, and then put them back
        in the bag. He'll do this a few times per game.
      - which games would have been possible if the bag contained only:
          12 red cubes, 13 green cubes, and 14 blue cubes?
      - Sum the IDs of the "possible" games.

      Part B
      - find the "fewest number of cubes of each color" per game
      - multiply them together ==> "power" of the set of cubes
      - What is the sum of the "power" of these sets?
-}

-- data Colors = Red | Green | Blue deriving (Show, Eq)

-- type CubeKV = M.Map Colors Int
type CubeKV = M.Map String Int

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (y,x)

toColorIntTuple :: [String] -> (String, Int)
toColorIntTuple = bimap id read . tuplify2

parseGameStrByHand :: [Char] -> M.Map String Int
parseGameStrByHand = foldl (M.unionWith max) M.empty
                     . map ( M.fromList
                           . map ( toColorIntTuple
                                 . words
                                 . dropWhile (==' ')
                                 )
                           . go ',' []
                         )
                         . go ';' []
                         . tail
  where
    go cond accu []  = accu
    go cond accu str = (\(f,l) ->
                        (if null l then f:accu else go cond (f:accu) (tail l)))
                        $ break (== cond) str

parseLineByHand :: [Char] -> (Int, CubeKV)
parseLineByHand = bimap (read . drop 5)
                        parseGameStrByHand
                        . break (== ':')

parseByHand :: String -> [(Int, CubeKV)]
parseByHand =
  map parseLineByHand . lines

filterPartA :: CubeKV -> Bool
filterPartA cube = cube == M.unionWith min compareCube cube
  where
    compareCube = M.fromList [("red",12),("green",13),("blue",14)]


partA :: String -> IO ()
partA filePath = do
  fileInput <- readFile filePath
  putStrLn "Day 02 - Part A"
  let games = parseByHand fileInput
  putStrLn $ unlines $ map show $ take 5 games
  hr
  let filteredGames = filter (\(_, cube) -> filterPartA cube) games
  putStrLn $ unlines $ map show $ take 5 filteredGames
  hr
  putStr "The sum of the IDs of valid games = "
  print $ sum $ map fst filteredGames


padZeroPartB :: CubeKV -> CubeKV
padZeroPartB = M.unionWith max compareCube
  where
    compareCube = M.fromList [("red",0),("green",0),("blue",0)]

partB :: String -> IO ()
partB filePath = do
  fileInput <- readFile filePath
  putStrLn "Day 02 - Part B"
  let games = parseByHand fileInput
  let five  = 5
  putStrLn $ "\t...first " <> show five
  putStrLn $ unlines $ map show $ take 5 games
  hr
  let powerSets = map (map (snd) . M.toList . padZeroPartB . snd) games
  -- hr
  putStrLn "The the \"power\" sets = "
  putStrLn $ "\t...first " <> show five
  putStrLn $ unlines $ map show $ take 5 powerSets
  putStr "The the sum of the \"power\" = "
  print $ sum $ map product powerSets

main :: IO ()
main = do
  -- let filePath = "input-02.test"
  let filePath = "input-02.txt"

  hr

  partA filePath

  hr

  partB filePath
