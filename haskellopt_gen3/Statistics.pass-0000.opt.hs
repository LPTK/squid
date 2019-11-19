-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  7
-- Incl. one-shot:   0
-- Case reductions:  16
-- Field reductions: 20
-- Case commutings:  6
-- Total nodes: 214; Boxes: 65; Branches: 32
-- Apps: 30; Lams: 6

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Statistics (avg,sum_cnt,lastWeird,lastMaybe,maxMaybe1,maxTest'0,maxMaybe0) where

import Data.Tuple.Select
import GHC.Classes
import GHC.Maybe
import GHC.Num
import GHC.Real
import GHC.Tuple
import GHC.Types

lam = \ds' -> case ds' of { (:) ρ ρ' -> (case ρ' of { [] -> Just ρ; _ -> (call ρ') }); [] -> Nothing }

sum_cnt = lam'

maxMaybe1 = lam'2

call' π = case (let (:) _ arg = π in arg) of { [] -> (let (:) arg _ = π in arg); _ -> (case (let (:) _ arg = π in arg) of { (:) ρ'2 ρ'3 -> (call' (let (:) _ arg = π in arg)); [] -> (666::Int) }) }

call'2 = 
  let ret = (call'3 True False (let (:) _ arg = undefined in arg)) in
  Just (case (1::Int) > ret of { True -> (1::Int); False -> ret })

lam'3 = \ls -> 
  let ret' = (call'4 ls) in
  div (sel2 ret') (sel1 ret')

call'5 π' = let
  _0 = Just (let (:) arg _ = π' in arg)
  ret'2 = (call'5 (let (:) _ arg = π' in arg))
  ψ = case (let (:) _ arg = π' in arg) of { (:) ρ'6 ρ'7 -> (case sel1 ret'2 of { Just ρ'8 -> (case (let (:) arg _ = π' in arg) > (let Just arg = sel2 ret'2 in arg) of { True -> Just (let (:) arg _ = π' in arg); False -> sel3 ret'2 }); Nothing -> _0 }); [] -> _0 }
  in (,,) ψ ψ (case π' of { (:) ρ'4 ρ'5 -> ψ; [] -> Nothing })

maxMaybe0 = lam'4

avg = lam'3

lam'5 = \ds'2 -> case ds'2 of { (:) ρ'9 ρ'10 -> Just (case ρ'10 of { [] -> ρ'9; _ -> (case ρ'10 of { (:) ρ'11 ρ'12 -> (call' ρ'10); [] -> (666::Int) }) }); [] -> Nothing }

lastMaybe = lam

call'6 π'2 = 
  let ret'3 = (call'6 (let (:) _ arg = π'2 in arg)) in
  (,) (case π'2 of { (:) ρ'13 ρ'14 -> sel1 ret'3 + (1::Int); [] -> (0::Int) }) (case π'2 of { (:) ρ'15 ρ'16 -> sel2 ret'3 + ρ'15; [] -> (0::Int) })

lam'4 = \ds -> 
  let ret = (call'3 False False (let (:) _ arg = ds in arg)) in
  case ds of { (:) ρ'17 ρ'18 -> Just (case ρ'18 of { (:) ρ'19 ρ'20 -> (case ρ'17 > ret of { True -> ρ'17; False -> ret }); [] -> ρ'17 }); [] -> Nothing }

lam'2 = \ds'3 -> let
  _1 = Just (let (:) arg _ = ds'3 in arg)
  ret'2 = (call'5 (let (:) _ arg = ds'3 in arg))
  in case ds'3 of { (:) ρ'21 ρ'22 -> (case ρ'22 of { (:) ρ'23 ρ'24 -> (case sel1 ret'2 of { Just ρ'25 -> (case ρ'21 > (let Just arg = sel2 ret'2 in arg) of { True -> Just ρ'21; False -> sel3 ret'2 }); Nothing -> _1 }); [] -> _1 }); [] -> Nothing }

lastWeird = lam'5

call π'3 = case π'3 of { (:) ρ'26 ρ'27 -> (case ρ'27 of { [] -> Just ρ'26; _ -> (call ρ'27) }); [] -> Nothing }

maxTest'0 = call'2

call'3 flag_ds flag_ds_ds _fε = let
  π'4 = case flag_ds_ds of { True -> (3::Int); False -> (case flag_ds of { True -> (2::Int); False -> (let (:) arg _ = _fε in arg) }) }
  ret = (call'3 False flag_ds (let (:) _ arg = _fε in arg))
  _fε' = case flag_ds of { True -> ret; False -> ret }
  ψ' = case π'4 > _fε' of { True -> π'4; False -> _fε' }
  in case flag_ds_ds of { True -> π'4; False -> (case flag_ds of { True -> ψ'; False -> (case (let (:) _ arg = _fε in arg) of { (:) ρ'28 ρ'29 -> ψ'; [] -> π'4 }) }) }

lam' = \ds'4 -> 
  let ret'3 = (call'6 (let (:) _ arg = ds'4 in arg)) in
  (,) (case ds'4 of { (:) ρ'30 ρ'31 -> sel2 ret'3 + ρ'30; [] -> (0::Int) }) (case ds'4 of { (:) ρ'32 ρ'33 -> sel1 ret'3 + (1::Int); [] -> (0::Int) })

call'4 ls' = 
  let ret'3 = (call'6 (let (:) _ arg = ls' in arg)) in
  (,) (case ls' of { (:) ρ'34 ρ'35 -> sel1 ret'3 + (1::Int); [] -> (0::Int) }) (case ls' of { (:) ρ'36 ρ'37 -> sel2 ret'3 + ρ'36; [] -> (0::Int) })
