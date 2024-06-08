-- ghc -O2 nofib-queens.hs && ./nofib-queens
-- ghc -O2 nofib-queens.pass-0000.opt.hs && ./nofib-queens.pass-0000.opt

import Criterion.Main

-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

import System.Environment


-- main = do
-- 	[arg] <- getArgs
-- 	print $ nsoln $ read arg

nsoln nq = length_mine (gen nq)
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: Int -> [[Int]]
    gen n | n == 0 = [[]]
    -- gen n = [ (q:b) | b <- gen (n-1), q <- [1..nq], safe q 1 b]
    gen n = [ (q:b) | b <- gen (n-1), q <- enumFromTo_mine 1 nq, safe q 1 b ]


--- Mine ---

nsoln :: Int -> Int

-- to simplify the graph, we can also try with the opaque defs:

length_mine [] = 0
length_mine (x : xs) = length_mine xs + 1
-- length_mine = length

enumFromTo_mine from to = go from where
  go from = if from > to then [] else from : go (from + 1)
-- enumFromTo_mine = enumFromTo


main = do
  defaultMain [bench "main" $ whnf (\n -> nsoln n) 8]
