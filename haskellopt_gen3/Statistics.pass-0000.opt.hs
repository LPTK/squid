-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  5
-- Incl. one-shot:   0
-- Case reductions:  12
-- Field reductions: 16
-- Case commutings:  6
-- Total nodes: 142; Boxes: 47; Branches: 28
-- Apps: 12; Lams: 4

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Statistics (lastWeird,lastMaybe,maxMaybe1,maxTest'0,maxMaybe0) where

import Data.Tuple.Select
import GHC.Classes
import GHC.Maybe
import GHC.Num
import GHC.Types

maxTest'0 = call

lam = \ds' -> case ds' of { (:) ρ ρ' -> (case ρ' of { [] -> Just ρ; _ -> (call' ρ') }); [] -> Nothing }

call'2 π = let
  _0 = Just (let (:) arg _ = π in arg)
  ψ = case (let (:) _ arg = π in arg) of { (:) ρ'4 ρ'5 -> (case sel1 (call'2 (let (:) _ arg = π in arg)) of { Just ρ'6 -> (case (let (:) arg _ = π in arg) > (let Just arg = sel3 (call'2 (let (:) _ arg = π in arg)) in arg) of { True -> Just (let (:) arg _ = π in arg); False -> sel2 (call'2 (let (:) _ arg = π in arg)) }); Nothing -> _0 }); [] -> _0 }
  in (,,) ψ (case π of { (:) ρ'2 ρ'3 -> ψ; [] -> Nothing }) ψ

lastWeird = lam'

lam'2 = \ds -> 
  let t = (call'3 False False (let (:) _ arg = ds in arg)) in
  case ds of { (:) ρ'7 ρ'8 -> Just (case ρ'8 of { (:) ρ'9 ρ'10 -> (case ρ'7 > t of { True -> ρ'7; False -> t }); [] -> ρ'7 }); [] -> Nothing }

call'3 flag_ds flag_ds_ds _fε = let
  π' = case flag_ds_ds of { True -> (3::Int); False -> (case flag_ds of { True -> (2::Int); False -> (let (:) arg _ = _fε in arg) }) }
  t' = (call'3 False flag_ds (let (:) _ arg = _fε in arg))
  _fε' = case flag_ds of { True -> t'; False -> t' }
  ψ' = case π' > _fε' of { True -> π'; False -> _fε' }
  in case flag_ds_ds of { True -> π'; False -> (case flag_ds of { True -> ψ'; False -> (case (let (:) _ arg = _fε in arg) of { (:) ρ'11 ρ'12 -> ψ'; [] -> π' }) }) }

maxMaybe1 = lam'3

lam' = \ds'2 -> case ds'2 of { (:) ρ'13 ρ'14 -> Just (case ρ'14 of { [] -> ρ'13; _ -> (case ρ'14 of { (:) ρ'15 ρ'16 -> (call'4 ρ'14); [] -> (666::Int) }) }); [] -> Nothing }

call' π'2 = case π'2 of { (:) ρ'17 ρ'18 -> (case ρ'18 of { [] -> Just ρ'17; _ -> (call' ρ'18) }); [] -> Nothing }

maxMaybe0 = lam'2

lam'3 = \ds'3 -> 
  let _1 = Just (let (:) arg _ = ds'3 in arg) in
  case ds'3 of { (:) ρ'19 ρ'20 -> (case ρ'20 of { (:) ρ'21 ρ'22 -> (case sel1 (call'2 ρ'20) of { Just ρ'23 -> (case ρ'19 > (let Just arg = sel3 (call'2 ρ'20) in arg) of { True -> Just ρ'19; False -> sel2 (call'2 ρ'20) }); Nothing -> _1 }); [] -> _1 }); [] -> Nothing }

call = 
  let t'2 = (call'3 True False (let (:) _ arg = undefined in arg)) in
  Just (case (1::Int) > t'2 of { True -> (1::Int); False -> t'2 })

lastMaybe = lam

call'4 π'3 = case (let (:) _ arg = π'3 in arg) of { [] -> (let (:) arg _ = π'3 in arg); _ -> (case (let (:) _ arg = π'3 in arg) of { (:) ρ'24 ρ'25 -> (call'4 (let (:) _ arg = π'3 in arg)); [] -> (666::Int) }) }
