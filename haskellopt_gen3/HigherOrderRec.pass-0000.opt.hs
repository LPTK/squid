-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  18
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 274; Boxes: 86; Branches: 49
-- Apps: 51; Lams: 15

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module HigherOrderp (s,r_2,r_0,r,only_q,q_1_0,q_1,q) where

import Data.Tuple.Select
import GHC.Base
import GHC.List
import GHC.Num
import GHC.Tuple
import GHC.Types

q_1_0 = lam

lam' f x = \y -> f x : (call x f (f y))

lam'2 f' = \x' -> (lam'3 f' x')

call' y' f'2 x'2 = f'2 x'2 : (call'2 x'2 f'2 (f'2 y'))

call'3 _0 f'3 = 
  let _1 = _0 + (1::Int) in
  _1 - (f'3 (call'3 _1 f'3) * f'3 (call'4 _1 f'3))

lam'4 = \f'4 -> (lam'5 f'4)

call'5 f'5 = f'5 (call'5 f'5)

lam'6 = \f'6 -> (lam'7 f'6)

r_0 = (lam'7 ((+) (1::Int)))

call'6 _2 = _2

call'7 f'7 = f'7

call'8 x'3 = x'3

call'9 _3 = _3

call'10 _4 = (lam'7 _4)

only_q = lam'8

lam'9 f'8 = \x'4 -> (lam' f'8 x'4)

call'11 _5 = (,) (lam'2 _5) _5

call'2 x'5 f'9 x'6 = f'9 x'6 : (call'2 x'6 f'9 (f'9 x'5))

lam'10 x'7 = \y'2 -> GHC.List.take (8::Int) (call' y'2 (sel2 (call'11 ((+) (1::Int)))) x'7)

q = lam'11

lam'11 = \f'10 -> (lam'2 f'10)

call'12 f'11 = f'11 (call'5 f'11)

call'13 f'12 = f'12

r_2 = GHC.List.take (3::Int) $ (call'12 ((:) (1::Int)))

lam'7 f'13 = \unit -> f'13 (call'5 f'13)

lam'3 f'14 x'8 = \y'3 -> f'14 x'8 : (call'2 x'8 f'14 (f'14 y'3))

call'4 _6 f'15 = 
  let _7 = _6 + (1::Int) in
  _7 - (f'15 (call'3 _7 f'15) * f'15 (call'4 _7 f'15))

lam'5 f'16 = \x'9 -> 
  let _8 = x'9 + (1::Int) in
  _8 - (f'16 (call'3 _8 f'16) * f'16 (call'4 _8 f'16))

s = lam'4

call'14 _9 = _9

r = lam'6

call'15 f'17 = f'17

lam'8 = \f'18 -> (lam'9 f'18)

call x'10 f'19 x'11 = f'19 x'11 : (call'2 x'11 f'19 (f'19 x'10))

lam = \x'12 -> (lam'10 x'12)

call'16 f'20 = f'20

q_1 = sel1 (call'11 ((+) (1::Int)))

call'17 f'21 = f'21
