-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  12
-- Incl. one-shot:   0
-- Case reductions:  10
-- Field reductions: 20
-- Case commutings:  0
-- Total nodes: 198; Boxes: 48; Branches: 61
-- Apps: 19; Lams: 4

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module PatMatRec (alternate'0,alternate,oops'0,oops,usum'2,usum'1,usum'0,usum) where

import Data.Tuple.Select
import GHC.List
import GHC.Num
import GHC.Tuple
import GHC.Types

lam = \ds -> case ds of { (:) ρ ρ' -> ρ + (call False False False ρ'); [] -> (0::Int) }

usum'1 = call'

call'2 π xs' xs'2 = let
  ret = (call'3 xs')
  ret' = (call'5 xs')
  in case π of { (:) ρ'2 ρ'3 -> ρ'2 + (call'2 ρ'3 (sel2 ret') (sel1 ret')); [] -> sel1 (call'4 False xs'2 (sel2 ret) (sel1 ret)) }

call'6 = (,) undefined []

lam' = \ds' -> (case ds' of { (,) ρ'4 ρ'5 -> ρ'4 }) : (case ds' of { (,) ρ'6 ρ'7 -> (call'7 ρ'6 ρ'7) })

call'8 = (1::Int) + (call True False False (let (:) _ arg = undefined in arg))

oops'0 = (call'9 (sel2 call'6) (sel1 call'6))

call'3 xs'3 = (,) xs'3 xs'3

alternate = lam'

lam'2 flag_xs xs'4 xs'5 = \ds'2 -> let
  ret = (call'3 xs'4)
  ret' = (call'5 xs'4)
  in case ds'2 of { (:) ρ'8 ρ'9 -> ρ'8 + (call'2 ρ'9 (sel2 ret') (sel1 ret')); [] -> sel1 (call'4 flag_xs xs'5 (sel2 ret) (sel1 ret)) }

call' = (1::Int) + (call False True False (let (:) _ arg = undefined in arg))

usum'2 = call'8

call'7 π' π'2 = π'2 : (call'7 π'2 π')

call'4 flag_xs' xs'6 xs'7 xs'8 = let
  ret = (call'3 xs'7)
  _cε = sel1 (call'4 False xs'8 (sel2 ret) (sel1 ret))
  ret' = (call'5 xs'7)
  in (,) (case flag_xs' of { True -> _cε; False -> (case xs'6 of { (:) ρ'10 ρ'11 -> ρ'10 + (call'2 ρ'11 (sel2 ret') (sel1 ret')); [] -> _cε }) }) _cε

usum = lam

usum'0 = (0::Int)

call'9 xs'9 xs'10 = 
  let ret = (call'3 xs'9) in
  sel2 (call'4 True xs'10 (sel2 ret) (sel1 ret))

lam'3 = \xs -> (lam'2 False xs xs)

call'5 xs'11 = (,) xs'11 xs'11

alternate'0 = GHC.List.take (5::Int) call'10

call flag_ds flag_ds' flag_ds_ds _fε = 
  let _0 = (case flag_ds of { True -> (2::Int); False -> (let (:) arg _ = _fε in arg) }) + (call False False flag_ds (let (:) _ arg = _fε in arg)) in
  case flag_ds' of { True -> (0::Int); False -> (case flag_ds_ds of { True -> (0::Int); False -> (case flag_ds of { True -> _0; False -> (case _fε of { (:) ρ'12 ρ'13 -> _0; [] -> (0::Int) }) }) }) }

oops = lam'3

call'10 = True : (call'7 True False)
