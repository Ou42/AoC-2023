module Main where
import Data.Bifunctor ( Bifunctor(bimap) )
import qualified Data.Map as M

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
-}

data Colors = Red | Green | Blue deriving Show

type CubeKV = M.Map Colors Int

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (y,x)

-- parseGameStrByHand :: String -> ???
parseGameStrByHand = map (map ( tuplify2
                              . words
                              . dropWhile (==' '))
                              . go ',' [])
                          . go ';' []
                          . tail
  where
    go cond accu []  = accu
    go cond accu str = (\(f,l) ->
                        (if null l then f:accu else go cond (f:accu) (tail l)))
                        $ break (== cond) str

-- ghci> parseGameStrByHand ": 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- [[("green","2")],[("blue","6"),("green","2"),("red","1")],[("red","4"),("blue","3")]]

parseLineByHand :: [Char] -> (Int, [Char])
parseLineByHand = bimap (read . drop 5)
                        tail -- parseGameStrByHand
                        . break (== ':')

parseByHand :: String -> String
parseByHand =
  show . map parseLineByHand . lines

partA :: String -> IO ()
partA filePath = do
  fileInput <- readFile filePath
  putStrLn "Day 02 - Part A"
  putStrLn (parseByHand fileInput)

main :: IO ()
main = do
  let filePath = "input-02.test"
  -- let filePath = "input-02.txt"

  hr

  partA filePath
