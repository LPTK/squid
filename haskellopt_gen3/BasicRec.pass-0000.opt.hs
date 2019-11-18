-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  42
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 544; Boxes: 126; Branches: 74
-- Apps: 135; Lams: 21

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module BasicRec (trec_0_1,trec_0_0,trec_0,trec_1,nrec_0,nrec_1,alternate123_3'0,alternate123_3,alternate123_2'0,alternate123_2,alternate123_1'0,alternate123_1,alternate123_0'0,alternate123_0,alternateZO_1'0,alternateZO_1,alternateZO_0'0,alternateZO_0,nrec_capt_0,alternateTF'0,alternateTF) where

import GHC.Base
import GHC.List
import GHC.Num
import GHC.Types

lam = \x -> (call (0::Int) x) ++ (call' x x)

call'2 = (0::Int)

lam' x' = \y -> (lam'2 x' y)

call'3 = (2::Int)

call'4 x'2 x'3 y' = x'3 : (call'4 x'3 y' x'2)

alternateZO_0 = lam'3

alternate123_2'0 = GHC.List.take (10::Int) (call'5 (2::Int) (1::Int))

lam'4 x'4 = \y'2 -> (lam'5 x'4 y'2)

alternateTF = 
  let _0 = True : (False : _0) in
  _0

alternate123_1 = lam'6

call'6 x'5 = x'5 : (call'7 x'5 (1::Int))

trec_0 = lam'7

call'8 x'6 = x'6 : (call'9 (x'6 + (1::Int)))

call'10 x'7 y'3 z x'8 = x'7 : (y'3 : (call'11 z y'3 z x'8))

call _1 x'9 = _1 : (call'12 (_1 + x'9) x'9)

lam'8 x'10 = \y'4 -> 
  let _2 = x'10 : (y'4 : _2) in
  _2

lam'9 x'11 = \y'5 -> (lam'10 y'5 x'11)

call'13 _3 = _3 : (call'13 (_3 + (1::Int)))

lam'11 x'12 = \y'6 -> (lam'12 x'12 y'6)

alternateZO_1'0 = GHC.List.take (5::Int) (call'14 (0::Int))

nrec_1 = lam'13

call'15 = (2::Int)

call'5 y'7 x'13 = (call'10 x'13 y'7 (3::Int) x'13)

call'16 = (1::Int)

call'17 _4 = _4 : (call'17 (_4 + (1::Int)))

lam'14 = \x'14 -> (call'8 x'14)

call'18 _5 = _5 : (call'19 (_5 + (1::Int)))

call'20 x'15 z' y'8 = x'15 : (y'8 : (z' : (call'21 x'15 z' y'8)))

nrec_0 = lam'14

lam'15 x'16 = \y'9 -> x'16 : (call'7 x'16 y'9)

lam'6 = \x'17 -> (lam'11 x'17)

alternate123_0 = lam'16

lam'17 = \x'18 -> (call'18 (0::Int)) ++ (call'22 x'18)

alternateZO_0'0 = GHC.List.take (5::Int) (call'6 (0::Int))

lam'18 = \x'19 -> (lam'4 x'19)

call'23 x'20 y'10 = x'20 : (call'4 x'20 y'10 (3::Int))

alternate123_3 = lam'18

call'24 _6 = _6 : (call'17 (_6 + (1::Int)))

call'25 = (1::Int)

lam'16 = \x'21 -> (lam' x'21)

call'26 y'11 = y'11

call'27 = (2::Int)

call'28 = (1::Int)

lam'19 = \x'22 -> (lam'8 x'22)

call'22 x'23 = x'23 : (call'19 (x'23 + (1::Int)))

lam'20 = \x'24 -> (lam'9 x'24)

trec_0_0 = (call'29 (27::Int)) ++ (call'24 (32::Int))

alternate123_1'0 = GHC.List.take (10::Int) (call'30 (1::Int) (2::Int))

call'31 y'12 = y'12

call'14 x'25 = 
  let _7 = x'25 : ((1::Int) : _7) in
  _7

alternateTF'0 = 
  let _8 = True : (False : _8) in
  GHC.List.take (5::Int) _8

call'32 _9 = _9 : (call'13 (_9 + (1::Int)))

call'33 x'26 y'13 = (call'20 x'26 (3::Int) y'13)

call'34 _10 = _10 : (call'17 (_10 + (1::Int)))

call' x'27 x'28 = x'27 : (call'12 (x'27 + x'28) x'28)

call'35 x'29 y'14 z'2 x'30 = x'29 : (y'14 : (call'11 z'2 y'14 z'2 x'30))

nrec_capt_0 = lam

lam'2 x'31 y'15 = \z'3 -> x'31 : (call'4 x'31 y'15 z'3)

lam'5 x'32 y'16 = \z'4 -> (call'20 x'32 z'4 y'16)

lam'7 = \y'17 -> y'17 : (call'17 (y'17 + (1::Int)))

trec_1 = lam'17

alternate123_0'0 = GHC.List.take (10::Int) (call'23 (1::Int) (2::Int))

lam'3 = \x'33 -> (lam'15 x'33)

lam'10 y'18 x'34 = \z'5 -> (call'10 x'34 y'18 z'5 x'34)

call'12 _11 x'35 = _11 : (call'12 (_11 + x'35) x'35)

call'7 x'36 x'37 = x'37 : (call'7 x'37 x'36)

call'11 z'6 y'19 z'7 x'38 = z'6 : (call'35 x'38 y'19 z'7 x'38)

call'9 _12 = _12 : (call'9 (_12 + (1::Int)))

call'36 = (1::Int)

alternateZO_1 = lam'19

alternate123_3'0 = GHC.List.take (10::Int) (call'33 (1::Int) (2::Int))

call'29 _13 = _13 : (call'17 (_13 + (1::Int)))

call'37 x'39 = x'39 : (call'13 (x'39 + (1::Int)))

lam'12 x'40 y'20 = \z'8 -> 
  let _14 = x'40 : (y'20 : (z'8 : _14)) in
  _14

call'38 = (2::Int)

call'30 x'41 y'21 = 
  let _15 = x'41 : (y'21 : ((3::Int) : _15)) in
  _15

lam'13 = \x'42 -> (call'32 (0::Int)) ++ (call'37 x'42)

call'21 x'43 z'9 y'22 = x'43 : (y'22 : (z'9 : (call'21 x'43 z'9 y'22)))

trec_0_1 = (call'39 (GHC.List.head (call'34 (32::Int))))

alternate123_2 = lam'20

call'40 = (0::Int)

call'41 z'10 = z'10

call'19 _16 = _16 : (call'19 (_16 + (1::Int)))

call'39 _17 = _17 : (call'17 (_17 + (1::Int)))
