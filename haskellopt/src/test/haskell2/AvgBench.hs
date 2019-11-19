module Main where

import Criterion.Main

sum_cnt :: [Int] -> (Int, Int)
sum_cnt [] = (0, 0)
sum_cnt (x : xs) = let (s, c) = sum_cnt xs in (s + x, c + 1)

avg ls = let (s, c) = sum_cnt ls in s `div` c

k = [0..1000*1000]

main = defaultMain [
    bench "avg"  $ nf avg k
  ]

{-

We can be faster if we use unboxed tuples,
but doing so is more eager and unless we nest the `let ret` doing the recursive call
into the relevant case branch, we get a crash (as it tires to access the tail of the empty list).


# Original:

benchmarking avg
time                 228.7 ms   (210.8 ms .. 255.2 ms)
                     0.996 R²   (0.994 R² .. 1.000 R²)
mean                 246.3 ms   (239.8 ms .. 257.5 ms)
std dev              10.97 ms   (4.836 ms .. 16.17 ms)
variance introduced by outliers: 16% (moderately inflated)


# Generated (using boxed tuples and `selN`):

benchmarking avg
time                 243.7 ms   (221.2 ms .. 256.8 ms)
                     0.998 R²   (0.990 R² .. 1.000 R²)
mean                 263.7 ms   (252.8 ms .. 276.8 ms)
std dev              14.44 ms   (8.994 ms .. 18.09 ms)
variance introduced by outliers: 16% (moderately inflated)


# Generated, unboxed tuple:

benchmarking avg
time                 166.4 ms   (161.4 ms .. 171.7 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 172.6 ms   (168.8 ms .. 178.3 ms)
std dev              6.612 ms   (1.385 ms .. 7.962 ms)
variance introduced by outliers: 12% (moderately inflated)

# ---

ghc -O2 AvgBench.hs && ./AvgBench
ghc -O2 bench/AvgBench.pass-0000.opt.hs && bench/AvgBench.pass-0000.opt

-}
