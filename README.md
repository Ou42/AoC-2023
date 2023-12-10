# My Advent of Code 2023 Journey!

## Using Haskell, running on Linux:

| TOOL       | VERSION  | via             | notes       |
| ---------- | -------- | --------------- | ----------- |
| GHCi       | 9.2.7    | ghci --version  | Day 01 & 02 |
| cabal      | 3.10.2.0 | cabal --version | Day 03      |
| cabal GHCi | 9.8.1    | cabal repl      | Day 03      |

## Completed: 

| DAY | Part A  | Part B  | ghci    | build tool | profiling |
| :-: | :-----: | :-----: | :-----: | :--------: | :-------: |
| 01  | &check; | &check; | &check; | _          | _         |
| 02  | &check; | &check; | &check; | _          | _         |
| 03  | &check; | &check; |         | cabal repl | _         |
| 04  | &check; |         |         | cabal repl | _         |

## Run / Build & Run Instructions:

```text
NOTE: copy & save input data into days/XX/input-XX.txt

$ cd days/XX

then:

$ ghci
λ :l day-XX.hs
λ main

-- or --

$ cabal repl
λ main

-- or --

$ cabal run <pkg name> && hp2ps -e8in -c <pkg name>.hp
```
