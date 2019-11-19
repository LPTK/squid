-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  2
-- Incl. one-shot:   0
-- Case reductions:  4
-- Field reductions: 4
-- Case commutings:  0
-- Total nodes: 160; Boxes: 32; Branches: 6
-- Apps: 50; Lams: 3

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main (main,avg,sum_cnt,k) where

import Data.Tuple.Select
import Control.DeepSeq
import Criterion.Main
import Criterion.Measurement.Types
import GHC.Base
import GHC.CString
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Tuple
import GHC.Types

main = Criterion.Main.defaultMain ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "avg"#) $ Criterion.Measurement.Types.nf lam (GHC.Enum.enumFromTo (0::Int) ((1000::Int) * (1000::Int)))) : [])

k = GHC.Enum.enumFromTo (0::Int) ((1000::Int) * (1000::Int))

sum_cnt = lam'

lam' = \ds -> 
  let ret = (call (let (:) _ arg = ds in arg)) in
  (,) (case ds of { (:) ρ ρ' -> sel1 ret + ρ; [] -> (0::Int) }) (case ds of { (:) ρ'2 ρ'3 -> sel2 ret + (1::Int); [] -> (0::Int) })

call π = 
  let ret = (call (let (:) _ arg = π in arg)) in
  (,) (case π of { (:) ρ'4 ρ'5 -> sel1 ret + ρ'4; [] -> (0::Int) }) (case π of { (:) ρ'6 ρ'7 -> sel2 ret + (1::Int); [] -> (0::Int) })

avg = lam

lam = \ls -> 
  let ret' = (call' ls) in
  div (sel1 ret') (sel2 ret')

call' ls' = 
  let ret = (call (let (:) _ arg = ls' in arg)) in
  (,) (case ls' of { (:) ρ'8 ρ'9 -> sel1 ret + ρ'8; [] -> (0::Int) }) (case ls' of { (:) ρ'10 ρ'11 -> sel2 ret + (1::Int); [] -> (0::Int) })
