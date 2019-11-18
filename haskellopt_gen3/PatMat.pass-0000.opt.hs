-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  38
-- Incl. one-shot:   1
-- Case reductions:  44
-- Field reductions: 55
-- Case commutings:  7
-- Total nodes: 1047; Boxes: 106; Branches: 285
-- Apps: 143; Lams: 17

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module PatMat (f0'f1',f0'f1,f1'2,f1'1,f1'0,f1,f0'4,f0'3,f0'2,f0'1,f0'0,f0,f2'0,slt0,t1'0,t0'0,u0_0,u1_0,vo'1,vo'0,v0,u1,u0,t1'1,t1,t0,t'ls,slt1,f2,orZero,e1'1,e1'0,e1,e0'3,e0'2,e0'1,e0'0,e0) where

import Control.Exception.Base
import Data.Foldable
import GHC.Base
import GHC.CString
import GHC.Classes
import GHC.Maybe
import GHC.Num
import GHC.Prim
import GHC.Tuple
import GHC.Types

lam = \ds -> case ds of { Just ρ -> ρ; Nothing -> (0::Int) }

lam' = \ds' -> case ds' of { (,) ρ' ρ'2 -> (case ρ'2 of { (,) ρ'3 ρ'4 -> (case ρ'4 of { (,) ρ'5 ρ'6 -> (case ρ'6 of { (,) ρ'7 ρ'8 -> (case ρ'8 of { () -> ((ρ' + ρ'3) + ρ'5) + ρ'7 }) }) }) }) }

lam'2 = \ds'2 -> case ds'2 of { (:) ρ'9 ρ'10 -> (case ρ'10 of { (:) ρ'11 ρ'12 -> (case ρ'12 of { (:) ρ'13 ρ'14 -> (case ρ'14 of { (:) ρ'15 ρ'16 -> (case ρ'16 of { [] -> ((ρ'9 + ρ'11) + ρ'13) + ρ'15; _ -> (666::Int) }); _ -> (666::Int) }); _ -> (666::Int) }); _ -> (666::Int) }); _ -> (666::Int) }

lam'3 = \x -> (call (call' x))

call'2 = lam'4

u1 = lam'

f1'1 = case call'3 of { True -> True; False -> False }

f2'0 = call'4

e1'1 = (0::Int)

t1 = lam'2

t0'0 = call'5

call'6 = Just ((0::Int) + (1::Int))

f0'4 = lam'3

lam'5 = \c -> (0::Int)

lam'6 π = \c' -> c' + π

e0'3 = (1::Int)

call'7 = Just ((2::Int) + (1::Int))

e0'0 = (2::Int) + (1::Int)

vo'1 = (0::Int)

f0'f1' = (call'8 call'9)

f0'1 = call'10

call'11 _0 = Just (case _0 of { True -> (42::Int) + (1::Int); False -> (0::Int) })

slt1 = lam'4

call'3 = (5::Int) > (0::Int)

call'12 _1 _2 = 
  let _3 = _2 (map (lam'6 _1) []) in
  (_3 * _3) + (1::Int)

f0'2 = (call'13 call'14)

lam'7 = \ds'3 -> case ds'3 of { (,,) ρ'17 ρ'18 ρ'19 -> (ρ'17 + ρ'18) + ρ'19 }

lam'8 = \ds'4 -> (0::Int)

u1_0 = call'15

t'ls = (1::Int) : ((2::Int) : ((3::Int) : ((4::Int) : [])))

e0'1 = (2::Int) + (1::Int)

lam'9 = \xs -> (call'16 xs)

v0 = lam'8

orZero = lam

f1'2 = (call'17 call'18)

f0 = lam'10

call'19 x' = 
  let _4 = Data.Foldable.sum (map (lam'6 x') []) in
  (_4 * _4) + (1::Int)

call'18 = (5::Int) > (0::Int)

call'16 xs' = case xs' of { (:) ρ'20 ρ'21 -> (case ρ'21 of { (:) ρ'22 ρ'23 -> (case ρ'23 of { [] -> (((5::Int) + (6::Int)) + ρ'20) + ρ'22; _ -> (666::Int) }); _ -> (666::Int) }); _ -> (666::Int) }

lam'11 = \x'2 -> case x'2 > (0::Int) of { True -> Just x'2; False -> Nothing }

lam'12 = \x'3 -> (,) (call'19 x'3) (call'12 (x'3 + (1::Int)) (Data.Foldable.sum . map lam'13))

call _ccε = Just (_ccε + (1::Int))

lam'4 = \ls -> map lam'5 (map lam'14 ls)

t1'0 = call'20

f1 = lam'11

e1 = Nothing

call' x'4 = case x'4 of { Just ρ'24 -> ρ'24 + (1::Int); Nothing -> (0::Int) }

t1'1 = lam'9

call'10 = Just (0::Int)

call'9 = (24::Int) > (0::Int)

call'15 = (((1::Int) + (2::Int)) + (3::Int)) + (4::Int)

u0 = lam'15

lam'16 = \ds'5 -> case ds'5 of { (:) ρ'25 ρ'26 -> (case ρ'26 of { (:) ρ'27 ρ'28 -> (case ρ'28 of { (:) ρ'29 ρ'30 -> (case ρ'30 of { (:) ρ'31 ρ'32 -> (case ρ'32 of { [] -> ((ρ'25 + ρ'27) + ρ'29) + ρ'31; _ -> (666::Int) }); _ -> (666::Int) }); _ -> (666::Int) }); _ -> (666::Int) }); _ -> (666::Int) }

lam'10 = \ds'6 -> Just (case ds'6 of { Just ρ'33 -> ρ'33 + (1::Int); Nothing -> (0::Int) })

e1'0 = (0::Int)

lam'14 = \c'2 -> (0::Int)

t0 = lam'16

u0_0 = call'21

f0'3 = call'6

lam'13 = \x'5 -> x'5 * (2::Int)

e0'2 = (2::Int) + (1::Int)

call'21 = (1::Int) + (2::Int)

call'14 = (3::Int) + (1::Int)

call'13 _5 = Just (_5 + (1::Int))

call'5 = (((1::Int) + (2::Int)) + (3::Int)) + (4::Int)

vo'0 = (0::Int)

f0'0 = call'7

lam'15 = \ds'7 -> case ds'7 of { (,) ρ'34 ρ'35 -> ρ'34 + ρ'35 }

call'4 = ((1::Int) + (2::Int)) + (3::Int)

call'17 _6 = case _6 of { True -> (5::Int); False -> (0::Int) }

e0 = Just (2::Int)

f0'f1 = (call'11 call'22)

f2 = lam'7

call'20 = (((1::Int) + (2::Int)) + (3::Int)) + (4::Int)

f1'0 = call'23

slt0 = lam'12

call'22 = (42::Int) > (0::Int)

call'23 = case (4::Int) > (0::Int) of { True -> Just (4::Int); False -> Nothing }

call'8 _7 = Just (case _7 of { True -> (24::Int) + (1::Int); False -> (0::Int) })
