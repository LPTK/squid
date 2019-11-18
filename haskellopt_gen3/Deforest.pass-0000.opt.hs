-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  6
-- Incl. one-shot:   0
-- Case reductions:  4
-- Field reductions: 8
-- Case commutings:  8
-- Total nodes: 140; Boxes: 43; Branches: 24
-- Apps: 19; Lams: 4

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Deforest (pgrm,sum_m,map_m) where

import Data.Tuple.Select
import GHC.Num
import GHC.Types

map_m = lam

lam = \f -> (lam' False f)

call ls f' = (,,,) (sel3 (call' True (let (:) _ arg = ls in arg) f')) (let (:) _ arg = ls in arg) (sel1 (call' True (let (:) _ arg = ls in arg) f')) ((let (:) arg _ = ls in arg) + (1::Int))

call'2 flag_ds π _0 t _cfε = 
  let _1 = (case flag_ds of { True -> t; False -> (let (:) arg _ = _cfε in arg) }) + (call'2 False undefined undefined undefined (case flag_ds of { True -> _0; False -> (let (:) _ arg = _cfε in arg) })) in
  case flag_ds of { True -> (case π of { (:) ρ ρ' -> _1; [] -> (0::Int) }); False -> (case _cfε of { (:) ρ'2 ρ'3 -> _1; [] -> (0::Int) }) }

sum_m = lam'2

lam'2 = \ds -> case ds of { (:) ρ'4 ρ'5 -> ρ'4 + (call'2 False undefined undefined undefined ρ'5); [] -> (0::Int) }

call'3 = undefined

lam'3 = \ls' -> (call'4 (sel2 (call ls' undefined)) (sel1 (call ls' undefined)) (sel4 (call ls' undefined)) (sel3 (call ls' undefined)) ls')

lam' flag_f f'2 = \ds' -> case ds' of { (:) ρ'6 ρ'7 -> (case flag_f of { True -> ρ'6 + (1::Int); False -> f'2 ρ'6 }) : sel2 (call' flag_f ρ'7 f'2); [] -> [] }

call' flag_f' π' f'3 = let
  ψ = sel2 (call' flag_f' (let (:) _ arg = π' in arg) f'3)
  t' = (let (:) arg _ = π' in arg) + (1::Int)
  in (,,) ψ (case π' of { (:) ρ'8 ρ'9 -> (case flag_f' of { True -> t'; False -> f'3 ρ'8 }) : ψ; [] -> [] }) t'

pgrm = lam'3

call'4 π'2 t'2 t'3 _2 ls'2 = case ls'2 of { (:) ρ'10 ρ'11 -> t'3 + (call'2 True π'2 _2 t'2 (let (:) _ arg = undefined in arg)); [] -> (0::Int) }
