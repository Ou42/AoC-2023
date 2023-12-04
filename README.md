# My Advent of Code 2023 Journey!

## Using Haskell, running on Linux:

| TOOL            | VERSION | via                    | notes       |
| :---            | :------ | :--                    | :----       |
| GHCi            | 9.2.7   | ghci --version         | Day 01 ...  |

## Completed: 

| DAY | Part A  | Part B  | ghci    | build tool | profiling |
| :-: | :-----: | :-----: | :-----: | :--------: | :-------: |
| 01  | &check; | &check; | &check; | _          | _         |
| 02  | &check; | &check; | &check; | _          | _         |
| 03  |         |         |         | _          | _         |

## Run / Build & Run Instructions:

```text
NOTE: copy & save input data into days/XX/input-XX.txt

$ cd days/XX

then:

$ ghci
λ :l day-XX.hs
λ main

-- or --

$ stack build
$ stack exec dayXX-exe

-- or --

$ stack build
$ ./dayXX-exe

-- or --

$ cabal run <pkg name> && hp2ps -e8in -c <pkg name>.hp
```
