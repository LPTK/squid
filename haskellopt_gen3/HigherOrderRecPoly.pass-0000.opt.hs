-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  20
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 705; Boxes: 178; Branches: 134
-- Apps: 143; Lams: 78

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module HigherOrderp (p1_6_5,p1_6,p1_5,p1_4,p1_3,p1_2,p1_1,p1,only_p2,only_p1,only_p0) where

import Data.Tuple.Select
import GHC.Base
import GHC.List
import GHC.Num
import GHC.Types

p1_4 = (call lam)

lam' = \f -> f (lam'2 f)

lam'3 = \h -> (lam'4 False undefined h)

lam'5 f' = \x -> (call' (id f')) x

call'2 = id (lam'2 id)

call f'2 = (lam'6 True f'2 undefined)

lam'7 flag_h' f'3 h'3 = \x' -> let
  _0 = x' + (1::Int)
  _1 = x' - (1::Int)
  in (case flag_h' of { True -> (call'3 _0 f'3); False -> h'3 _0 }) * (case flag_h' of { True -> (call'4 _1 f'3); False -> h'3 _1 })

p1_1 = call'2

call'5 f'4 = f'4 (lam'8 f'4)

lam = \h'2 -> (lam'6 False undefined h'2)

only_p1 = lam'9

p1_6_5 = lam'10

call'6 _2 f'5 = (call'7 (id f'5)) (_2 + (1::Int))

call'8 λ = (call λ)

call'3 _3 f'6 = (call'7 (id f'6)) (_3 + (1::Int))

lam'8 f'7 = \x'2 -> (call'5 f'7) x'2

lam'11 = \h'4 -> h'4

call'9 λ' = (call'10 λ')

call'10 f'8 = (lam'7 True f'8 undefined)

lam'10 = \a -> GHC.List.take (5::Int) (call'11 a (sel2 (call'12 lam'3)))

p1_5 = (call'10 lam'12)

call'13 λ'2 = (lam'2 λ'2)

lam'9 = \f'9 -> f'9 (lam'5 f'9)

p1_2 = (lam'2 lam'11)

lam'2 f'10 = \x'3 -> (call'7 (id f'10)) (x'3 + (1::Int))

call' _4 = _4 (lam'5 _4)

lam'6 flag_h'2 f'11 h'5 = \x'4 -> case flag_h'2 of { True -> (call'14 x'4 f'11); False -> h'5 x'4 }

call'15 f'12 = (lam'4 True f'12 undefined)

call'16 = lam'13

only_p2 = lam'14

lam'15 = \f'13 -> f'13 (lam'8 f'13)

call'17 _5 = _5 (call'17 (id _5))

call'14 x'5 f'14 = (call'7 (id f'14)) (x'5 + (1::Int))

p1 = lam'

lam'12 = \h' -> (lam'7 False undefined h')

call'11 a' f'15 = a' : (call'6 (a' + (1::Int)) f'15)

p1_6 = sel1 (call'12 lam'3)

call'7 _6 = _6 (lam'2 _6)

lam'13 = \x'6 -> x'6

only_p0 = lam'15

lam'14 = \f'16 -> f'16 (call'17 (id f'16))

call'4 _7 f'17 = (call'7 (id f'17)) (_7 + (1::Int))

call'18 = lam'13

p1_3 = lam'13

lam'4 flag_h f'18 h'6 = \x'7 -> 
  let _8 = x'7 + (1::Int) in
  x'7 : (case flag_h of { True -> (call'6 _8 f'18); False -> h'6 _8 })

call'12 λ'3 = (,) (call'15 λ'3) λ'3
