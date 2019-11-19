-- Lots of indirection by going through Fold... how much of that can we eliminate?

{-# LANGUAGE PatternSynonyms #-}

module Main where

import Criterion.Main


-- Monomorphized Control.Foldl for integers and pairs to get around GHC type inference problems in generated code

type FoldI a b = ((Int -> a -> Int), Int, (Int -> b))
-- pattern FoldI a b c = (a, b, c)

type FoldP a b = (((Int,Int) -> a -> (Int,Int)), (Int,Int), ((Int,Int) -> b))
-- pattern FoldP a b c = (a, b, c)

ap_F :: FoldI x (a -> b) -> FoldI x a -> FoldP x b
((,,) stepL beginL doneL) `ap_F` ((,,) stepR beginR doneR) =
    let step ((,) xL xR) a = (,) (stepL xL a) (stepR xR a)
        begin = (,) beginL beginR
        done ((,) xL xR) = doneL xL (doneR xR)
    in  (,,) step begin done

fmap_F f ((,,) step begin done) = (,,) step begin (\x -> f (done x)) -- (f . done)

-- fold :: Foldable f => Fold a b -> f a -> b
-- fold (Fold step begin done) as = F.foldr cons done as begin
--   where
--     cons a k x = k $! step x a
fold_F :: FoldP a b -> [a] -> b
fold_F ((,,) step begin done) xs = done (foldl' step begin xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
-- TOOD use `go` version below
foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x 
                    -- in seq z' $ foldl' f z' xs
                    in foldl' f z' xs
-- foldl' f = go where
--     go z []     = z
--     go z (x:xs) = let z' = z `f` x
--                   in go z' xs

-- sum :: Num a => Fold a a
sum_F :: FoldI Int Int
sum_F = (,,) (+) 0 id

-- genericLength :: Num b => Fold a b
length_F :: FoldI a Int
length_F = (,,) (\n _ -> n + 1) 0 id





avg_F xs = let
        f = (,) `fmap_F` sum_F `ap_F` length_F
        (s,l) = fold_F f xs
    in s `div` l

avg_manual_tr :: [Int] -> Int
avg_manual_tr xs = go 0 0 xs where
    go s l [] = s `div` l
    go s l (x : xs) = go (s + x) (l + 1) xs

avg_manual :: [Int] -> Int
avg_manual xs = su `div` le where
    (su, le) = go xs
    go [] = (0, 0)
    go (x : xs) =
        let (s, l) = go xs
        in (s + x, l + 1)

k = [0..1000*1000]

main = defaultMain [
    bgroup "folding" [
        bench "normal"  $ whnf avg_F k,
        bench "manual"  $ whnf avg_manual_tr k,
        bench "manual_tr"  $ whnf avg_manual_tr k
    ]
  ]

{-

> ghc -O2 -fforce-recomp -ddump-to-file -ddump-prep -dsuppress-module-prefixes -dsuppress-idinfo FoldingBench.hs && ./FoldingBench
[1 of 1] Compiling Main             ( FoldingBench.hs, FoldingBench.o )
Linking FoldingBench ...
benchmarking folding/normal
time                 239.7 ms   (211.6 ms .. 260.6 ms)
                     0.996 R²   (0.989 R² .. 1.000 R²)
mean                 241.3 ms   (235.8 ms .. 248.5 ms)
std dev              8.002 ms   (4.429 ms .. 11.41 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking folding/manual
time                 3.089 ms   (3.004 ms .. 3.216 ms)
                     0.993 R²   (0.989 R² .. 0.996 R²)
mean                 3.161 ms   (3.094 ms .. 3.250 ms)
std dev              255.9 μs   (189.7 μs .. 368.3 μs)
variance introduced by outliers: 56% (severely inflated)

benchmarking folding/manual_tr
time                 3.009 ms   (2.904 ms .. 3.119 ms)
                     0.988 R²   (0.979 R² .. 0.994 R²)
mean                 3.160 ms   (3.074 ms .. 3.278 ms)
std dev              324.4 μs   (238.7 μs .. 418.4 μs)
variance introduced by outliers: 66% (severely inflated)

-}
