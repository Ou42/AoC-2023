module Main where
import Data.ByteString (fromFilePath)
import Data.Bifunctor ( Bifunctor(bimap) )

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

data Cubes a = Red a | Green a | Blue a deriving Show

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

parseLineByHand :: [Char] -> (Int, [Char])
-- parseLineByHand = break (== ':')
-- Q: why ... `tail . break` ... and not `$`?! ===>>> A: due to eta reduction!
parseLineByHand = bimap (read . drop 5) tail . break (== ':')
-- parseLineByHand line = bimap (drop 5) tail $ break (== ':') line

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
