-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  5
-- Incl. one-shot:   0
-- Case reductions:  2
-- Field reductions: 2
-- Case commutings:  2
-- Total nodes: 174; Boxes: 33; Branches: 10
-- Apps: 48; Lams: 4

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main (main,maxMaybe,maxMaybe_manual,k) where

import Control.DeepSeq
import Criterion.Main
import Criterion.Measurement.Types
import GHC.Base
import GHC.CString
import GHC.Classes
import GHC.Enum
import GHC.Maybe
import GHC.Num
import GHC.Types

k = GHC.Enum.enumFromTo (0::Int) ((1000::Int) * (1000::Int))

call ψ = ψ

main = 
  let _0 = GHC.Enum.enumFromTo (0::Int) ((1000::Int) * (1000::Int)) in
  Criterion.Main.defaultMain (Criterion.Measurement.Types.bgroup (GHC.CString.unpackCString# "maxMaybe"#) ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "normal"#) $ Criterion.Measurement.Types.nf lam _0) : ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "manual"#) $ Criterion.Measurement.Types.nf lam' _0) : [])) : [])

call' π x = case π of { (:) ρ ρ' -> (call' ρ' (case ρ > x of { True -> ρ; False -> x })); [] -> x }

maxMaybe_manual = lam'

call'2 π' = π'

lam' = \ds -> case ds of { (:) ρ'2 ρ'3 -> Just (call'3 ρ'3 ρ'2); [] -> Nothing }

lam = \ds' -> 
  let ret = (call'4 (let (:) _ arg = ds' in arg)) in
  case ds' of { (:) ρ'4 ρ'5 -> Just (case ρ'5 of { (:) ρ'6 ρ'7 -> (case ρ'4 > ret of { True -> ρ'4; False -> ret }); [] -> ρ'4 }); [] -> Nothing }

call'3 π'2 x' = case π'2 of { (:) ρ'8 ρ'9 -> (call' ρ'9 (case ρ'8 > x' of { True -> ρ'8; False -> x' })); [] -> x' }

maxMaybe = lam

call'4 π'3 = 
  let ret = (call'4 (let (:) _ arg = π'3 in arg)) in
  case (let (:) _ arg = π'3 in arg) of { (:) ρ'10 ρ'11 -> (case (let (:) arg _ = π'3 in arg) > ret of { True -> (let (:) arg _ = π'3 in arg); False -> ret }); [] -> (let (:) arg _ = π'3 in arg) }
