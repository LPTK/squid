-- Generated Haskell code from SynthBench

module Main where

import Criterion.Main

prod_14 :: Num a => [a] -> a
prod_14 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13] = 1 * x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10 * x11 * x12 * x13
prod_14 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12] = 1 * x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10 * x11 * x12
prod_14 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] = 1 * x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10 * x11
prod_14 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10] = 1 * x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10
prod_14 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = 1 * x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9
prod_14 [x0,x1,x2,x3,x4,x5,x6,x7,x8] = 1 * x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8
prod_14 [x0,x1,x2,x3,x4,x5,x6,x7] = 1 * x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7
prod_14 [x0,x1,x2,x3,x4,x5,x6] = 1 * x0 * x1 * x2 * x3 * x4 * x5 * x6
prod_14 [x0,x1,x2,x3,x4,x5] = 1 * x0 * x1 * x2 * x3 * x4 * x5
prod_14 [x0,x1,x2,x3,x4] = 1 * x0 * x1 * x2 * x3 * x4
prod_14 [x0,x1,x2,x3] = 1 * x0 * x1 * x2 * x3
prod_14 [x0,x1,x2] = 1 * x0 * x1 * x2
prod_14 [x0,x1] = 1 * x0 * x1
prod_14 [x0] = 1 * x0
prod_14 [] = 1
test_14 n = sum (map (\i -> prod_14 [i ^ 0, i ^ 1, i ^ 2, i ^ 3, i ^ 4, i ^ 5, i ^ 6, i ^ 7, i ^ 8, i ^ 9, i ^ 10, i ^ 11, i ^ 12, i ^ 13]) [0..n])

main = defaultMain [
    bench "14" $ whnf test_14 1000
  ]
