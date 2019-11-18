-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  32
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 445; Boxes: 148; Branches: 135
-- Apps: 76; Lams: 7

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Explosive (a2,a1,a0,b1,b0,a3) where

import GHC.Num
import GHC.Real
import GHC.Types

call _0 = ((1::Int) - _0) - (1::Int)

lam flag_f flag_f' g f' f'2 = \x -> let
  _2 = case flag_f of { True -> (call'5 x f'); False -> (case flag_f' of { True -> (call'6 x g); False -> f'2 x }) }
  _1 = case flag_f' of { True -> (call'3 _2 g); False -> (case flag_f of { True -> (call'4 _2 f'); False -> f'2 _2 }) }
  in case flag_f' of { True -> (call' _1 g); False -> (case flag_f of { True -> (call'2 _1 f'); False -> f'2 _1 }) }

a2 = (call'7 ((+) call'8)) ^ ((call'9 ((*) call'10)) ^ (call'11 ((^) call'12)))

call'2 _3 f'3 = f'3 (f'3 (f'3 _3))

b1 = lam'

call'11 _4 = ((call'13 _4) * (call'14 _4)) * (call'15 _4)

call'16 _5 f'4 = (call'2 (call'4 (call'5 _5 f'4) f'4) f'4)

call'17 y f'5 = f'5 (f'5 (f'5 y))

lam'2 = \g' -> ((call'13 g') * (call'14 g')) * (call'15 g')

call'8 = ((call'18 (0::Int)) + (call'19 (1::Int))) + (call'20 (2::Int))

call'10 = ((call'21 (0::Int)) + (call (1::Int))) + (call'22 (2::Int))

call'14 g'2 = (g'2 (0::Int) + g'2 (1::Int)) + g'2 (2::Int)

call'3 _6 g'3 = (call'16 (call'23 (call'17 _6 g'3) g'3) g'3)

lam' = \g'4 -> (call'24 g'4)

call'25 g'5 = g'5

call'20 _7 = ((1::Int) - _7) - (1::Int)

call'21 _8 = ((1::Int) - _8) - (1::Int)

call'26 _9 = ((1::Int) - _9) - (1::Int)

call'5 x' f'6 = f'6 (f'6 (f'6 x'))

call'9 _10 = ((call'13 _10) * (call'14 _10)) * (call'15 _10)

lam'3 = \x'2 -> ((1::Int) - x'2) - (1::Int)

a0 = lam'4

call'22 _11 = ((1::Int) - _11) - (1::Int)

call' _12 g'6 = (call'16 (call'23 (call'17 _12 g'6) g'6) g'6)

a3 = lam'3

call'13 g'7 = (g'7 (0::Int) + g'7 (1::Int)) + g'7 (2::Int)

call'23 _13 f'7 = f'7 (f'7 (f'7 _13))

call'18 _14 = ((1::Int) - _14) - (1::Int)

call'6 x'3 g'8 = (call'16 (call'23 (call'17 x'3 g'8) g'8) g'8)

call'4 _15 f'8 = f'8 (f'8 (f'8 _15))

call'24 g'9 = (lam False True g'9 undefined undefined)

b0 = lam'5

call'27 g'10 = g'10

call'19 _16 = ((1::Int) - _16) - (1::Int)

call'12 = ((call'28 (0::Int)) + (call'26 (1::Int))) + (call'29 (2::Int))

lam'4 = \f'9 -> (f'9 (0::Int) + f'9 (1::Int)) + f'9 (2::Int)

call'28 _17 = ((1::Int) - _17) - (1::Int)

lam'5 = \f -> (lam False False undefined undefined f)

call'30 g'11 = g'11

a1 = lam'2

call'15 g'12 = (g'12 (0::Int) + g'12 (1::Int)) + g'12 (2::Int)

call'29 _18 = ((1::Int) - _18) - (1::Int)

call'7 _19 = ((call'13 _19) * (call'14 _19)) * (call'15 _19)
