-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  14
-- Incl. one-shot:   0
-- Case reductions:  62
-- Field reductions: 85
-- Case commutings:  78
-- Total nodes: 1489; Boxes: 530; Branches: 564
-- Apps: 87; Lams: 3

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module IterEither (count,simple9,simple5,simple1,loop) where

import Data.Tuple.Select
import Data.Either
import GHC.Classes
import GHC.Num
import GHC.Tuple
import GHC.Types

simple1 = (20::Int)

call flag_x'_x'_x_k flag_x'_x'_x_k' flag_x'_x'_x_k'2 flag_x_x' flag_x_x'' flag_x_x''2 flag_x'_k flag_x'_x'_x_k'3 _0 x'2 π _1 _2 x'3 = 
  let π' = case flag_x_x'' of { True -> _0; False -> (case flag_x_x' of { True -> True; False -> (case flag_x_x''2 of { True -> (20::Int); False -> (case flag_x'_x'_x_k'2 of { True -> π; False -> (case flag_x'_k of { True -> (let (,) _ arg = _2 in arg); False -> (case flag_x'_x'_x_k' of { True -> _1; False -> (case flag_x'_x'_x_k'3 of { True -> _1; False -> (case flag_x'_x'_x_k of { True -> _1; False -> (let (,) _ arg = x'3 in arg) }) }) }) }) }) }) }) } in
  (,) π' (case flag_x_x' of { True -> π'; False -> (case flag_x_x''2 of { True -> π'; False -> (case flag_x'_x'_x_k'2 of { True -> π'; False -> (case flag_x'_k of { True -> (case _2 of { (,) ρ ρ' -> π' }); False -> (case flag_x'_x'_x_k' of { True -> π'; False -> (case flag_x'_x'_x_k'3 of { True -> π'; False -> (case flag_x'_x'_x_k of { True -> π'; False -> (case x'2 of { (,) ρ'2 ρ'3 -> π' }) }) }) }) }) }) }) })

lam = \k -> (lam' False)

call' flag_x'_k' flag_x'_x'_x_k'4 flag_x'_x'_x_k'5 flag_x_x''3 flag_x_x''4 flag_x_x''5 flag_x'_x'_x_k'6 flag_x'_x'_x_k'7 π'2 x'4 _3 _4 start = (case flag_x_x''4 of { True -> start; False -> (case flag_x_x''3 of { True -> False; False -> (case flag_x_x''5 of { True -> (10::Int); False -> (case flag_x'_x'_x_k'5 of { True -> π'2; False -> (case flag_x'_x'_x_k'6 of { True -> _3; False -> (case flag_x'_x'_x_k'4 of { True -> _3; False -> (case flag_x'_x'_x_k'7 of { True -> _3; False -> (case flag_x'_k' of { True -> (let (,) arg _ = _4 in arg); False -> (let (,) arg _ = x'4 in arg) }) }) }) }) }) }) }) }) > (0::Int)

count = lam'2

simple5 = call'2

call'3 start' _5 _6 = sel3 (call'4 False False False True False False start' undefined undefined _5 undefined _6)

call'4 flag_k_x flag_k_x' flag_x flag_x' flag_x'2 flag_k start'2 x'5 x'6 x'7 x'8 _7 = 
  let π'3 = case flag_k_x' of { True -> sel1 (call False False False flag_x flag_x' flag_x'2 False False _7 x'6 undefined undefined undefined x'5); False -> (case flag_k of { True -> sel2 (call False False False flag_x flag_x' flag_x'2 False False _7 x'6 undefined undefined undefined x'5); False -> undefined }) } in
  (,,) (case (call'5 x'7) of { True -> undefined; False -> π'3 }) (case flag_k_x' of { True -> π'3; False -> (case flag_k of { True -> π'3; False -> (case flag_k_x of { True -> undefined; False -> undefined }) }) }) (case (call' False False False flag_x flag_x' flag_x'2 False False undefined x'8 undefined undefined start'2) of { True -> undefined; False -> π'3 })

call'5 x'9 = x'9 > (0::Int)

lam'2 = \start'3 -> (call'3 start'3 ((,) start'3 (0::Int)) (0::Int))

loop = lam

lam' flag_k' = \x -> sel2 (call'4 undefined undefined False False False flag_k' undefined x x x x undefined)

call'6 = undefined

call'2 = sel1 (call'4 False False False False False False undefined (5::Int) (5::Int) (5::Int) (5::Int) undefined)

simple9 = False
