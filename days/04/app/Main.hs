module Main where

import qualified Data.List as L
import qualified Data.Set as S
import Data.Set (Set)

{-
    Day 04

      Part A
      - given a list of numbered Cards
      - each card conatins 2 lists:
        - a list of winning numbers
        - a list of numbers we have
      - figure out which of the numbers you have appear in the list of winning numbers
      - Score 1 point for the first match, then double the points for ea match after the 1st
      - What is the total number of points?

      Part B
      - instead of points ...
      - scratchcards only cause you to win more scratchcards
        - equal to the number of winning numbers you have.
      - you win copies of the scratchcards below the winning card equal to the number of matches.
        - So, if card 10 were to have 5 matching numbers, you would win one copy each
          of cards 11, 12, 13, 14, and 15.

      - Process all of the original and copied scratchcards until no more scratchcards are won.
        Including the original set of scratchcards, how many total scratchcards do you end up with?
-}
type Card = ([Char], (Set Int, Set Int))

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

parseDay04 :: String -> [Card]
parseDay04 fileInput =  L.map ( fmap ( go
                                     . fmap (L.drop 1)
                                     . break (== '|')
                                     . L.drop 1 )
                              . break (== ':') 
                              . L.drop 4)
                        $ lines fileInput
  where
    go :: (String, String) -> (Set Int, Set Int)
    go (win, picked) = ( S.fromList $ L.map read $ words win
                       , S.fromList $ L.map read $ words picked )


partA :: [Card] -> Int
partA = sum
        . L.map (snd . fmap ((\pow -> 2^(pow-1)) . S.size))
        . L.filter (\(_,s) -> not $ S.null s)
        . L.map (\(cardNo,(win, picked)) -> (cardNo, S.intersection win picked)) 

data CardInfo = CardInfo { extra :: [Int], qty :: Int } deriving (Show)

-- partB :: [Card] -> [_]
partB = L.map (\(cardNo,(win, picked)) -> let cardInt = read cardNo :: Int
                                              extra = (cardInt+) <$> [1..length (S.intersection win picked)]
                                              qty   = 1
                                          in (cardInt, CardInfo extra qty ) )


main :: IO ()
main = do
  fileInput <- readFile "input-04.test"
  -- fileInput <- readFile "input-04.txt"

  let parsedData = parseDay04 fileInput
--   putStrLn $ unlines $ L.map show parsedData

  hr

  -- part A using test data: ... answer should be 13 points
  putStr "The answer for Day 04 - Part A = "
  print $ partA parsedData

  -- part B using test data: ... answer should be 30 scratchcards
  putStr "The answer for Day 04 - Part B = "
  -- print $ partB parsedData

  putStrLn $ unlines $ "\n" : (map show $ partB parsedData)
