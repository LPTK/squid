-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  28
-- Incl. one-shot:   0
-- Case reductions:  34
-- Field reductions: 29
-- Case commutings:  0
-- Total nodes: 555; Boxes: 96; Branches: 136
-- Apps: 79; Lams: 8

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module PatMatRec (t2_,t2'0'5,t2'1'5,t2'1,t2'0,t2,t1_,t1'2,t1'1,t1'0,t1,t0_,t0'1,t0'0,t0) where

import Data.Tuple.Select
import GHC.List
import GHC.Num
import GHC.Prim
import GHC.Tuple
import GHC.Types

lam = \ds -> case ds of { (:) ρ ρ' -> (case ρ' of { [] -> call; _ -> (666::Int) }); _ -> (666::Int) }

t2'0 = sel2 call'

t0'0 = call

call'2 = sel1 (call'3 True False undefined undefined)

t1'2 = call'2

call' = 
  let _0 = (0::Int) - (1::Int) in
  (,,) _0 (_0 : sel2 (call'4 (1::Int) (0::Int))) (sel1 (call'4 (1::Int) (0::Int)))

lam' = \ds' -> case ds' of { (:) ρ'2 ρ'3 -> (case ρ'3 of { (:) ρ'4 ρ'5 -> (case ρ'5 of { [] -> ρ'2 - ρ'4; _ -> (666::Int) }) : (case ρ'5 of { [] -> sel2 (call'4 ρ'4 ρ'2); _ -> [] }); _ -> call'5 }); _ -> call'6 }

call'7 _1 = _1 : (call'8 (_1 + (1::Int)))

lam'2 = \xs -> sel2 (call'3 False False undefined xs)

t1 = lam'3

t2'0'5 = GHC.List.take (5::Int) (sel2 call')

call'9 = call

call'6 = (666::Int) : []

lam'4 = \x -> (call'10 x)

lam'5 = \ds'2 -> case ds'2 of { (:) ρ'6 ρ'7 -> (case ρ'7 of { [] -> call; _ -> (666::Int) }); _ -> (666::Int) }

t0'1 = (666::Int)

call'11 = (666::Int) : []

call'5 = (666::Int) : []

t2'1 = (,) (sel1 call') (sel3 call')

lam'6 = \ds'3 -> (case ds'3 of { (:) ρ'8 ρ'9 -> (case ρ'9 of { (:) ρ'10 ρ'11 -> (case ρ'11 of { [] -> ρ'8 - ρ'10; _ -> (666::Int) }); _ -> (666::Int) }); _ -> (666::Int) }) : (case ds'3 of { (:) ρ'12 ρ'13 -> (case ρ'13 of { (:) ρ'14 ρ'15 -> (case ρ'15 of { [] -> (call'12 ρ'14 ρ'12); _ -> [] }); _ -> [] }); _ -> [] })

t1_ = lam'7

t1'0 = lam'2

call'4 π π' = 
  let _2 = π - π' in
  (,) _2 (_2 : sel2 (call'4 π' π))

call'10 x' = sel1 (call'3 False True x' undefined)

t2_ = lam'6

lam'7 = \ds'4 -> (case ds'4 of { (:) ρ'16 ρ'17 -> (case ρ'17 of { [] -> ρ'16; _ -> (666::Int) }); _ -> (666::Int) }) : (case ds'4 of { (:) ρ'18 ρ'19 -> (case ρ'19 of { [] -> (call'7 (ρ'18 + (1::Int))); _ -> [] }); _ -> [] })

call'13 = call

call'3 flag_xs flag_xs' x'2 xs' = let
  _fε = case flag_xs of { True -> (0::Int); False -> (case flag_xs' of { True -> x'2; False -> (let (:) arg _ = xs' in arg) }) }
  _cε' = (call'8 (_fε + (1::Int)))
  _cε = (case flag_xs' of { True -> _fε; False -> (case flag_xs of { True -> _fε; False -> (case (let (:) _ arg = xs' in arg) of { [] -> _fε; _ -> (666::Int) }) }) }) : (case flag_xs' of { True -> _cε'; False -> (case flag_xs of { True -> _cε'; False -> (case (let (:) _ arg = xs' in arg) of { [] -> _cε'; _ -> [] }) }) })
  in (,) _cε (case flag_xs of { True -> _cε; False -> (case flag_xs' of { True -> _cε; False -> (case xs' of { (:) ρ'20 ρ'21 -> _cε; _ -> call'11 }) }) })

t1'1 = lam'4

call = call

lam'3 = \ds'5 -> case ds'5 of { (:) ρ'22 ρ'23 -> (case ρ'23 of { [] -> ρ'22; _ -> (666::Int) }) : (case ρ'23 of { [] -> (call'8 (ρ'22 + (1::Int))); _ -> [] }); _ -> call'11 }

call'8 _3 = _3 : (call'8 (_3 + (1::Int)))

t0_ = lam'5

t2'1'5 = sel1 call' * sel3 call'

t0 = lam

t2 = lam'

call'12 π'2 π'3 = (π'2 - π'3) : sel2 (call'4 π'3 π'2)
