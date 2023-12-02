module Main where

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

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']


main :: IO ()
main = do
  fileInput <- readFile "input-01.test"
  -- fileInput <- readFile "input-01.txt"

  putStrLn "Day 02 - Part A"
  putStrLn $ unlines $ take 3 $ lines fileInput

  hr
