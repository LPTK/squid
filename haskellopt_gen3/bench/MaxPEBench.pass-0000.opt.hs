-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  10
-- Incl. one-shot:   0
-- Case reductions:  10
-- Field reductions: 15
-- Case commutings:  2
-- Total nodes: 429; Boxes: 107; Branches: 70
-- Apps: 101; Lams: 10

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main (main,max_mod,k,max3,maxMaybe,max3_manual) where

import Control.DeepSeq
import Control.Exception.Base
import Criterion.Main
import Criterion.Measurement.Types
import GHC.Base
import GHC.CString
import GHC.Classes
import GHC.Enum
import GHC.Maybe
import GHC.Num
import GHC.Real
import GHC.Tuple
import GHC.Types

lam x = \y -> (lam' y x)

call v = v

lam'2 = \m -> map (lam'3 False False m)

max_mod = lam'2

call' y' x' z = 
  let t = (call'2 True False y' z (let (:) _ arg = undefined in arg) undefined) in
  case x' > t of { True -> x'; False -> t }

lam'4 x'2 y'2 = \z' -> (call' y'2 x'2 z')

call'3 v' = v'

maxMaybe = lam'5

max3 = lam'6

call'4 = map (lam'3 True False undefined)

lam'7 = \x'3 -> (lam x'3)

call'5 _0 x'4 y'3 = (call' y'3 x'4 _0)

lam'3 flag_m flag_m' m' = \v'2 -> let
  _2 = mod v'2 (5::Int)
  _1 = mod v'2 (10::Int)
  in case flag_m' of { True -> (call' v'2 _1 _2); False -> (case flag_m of { True -> (call'6 _2 v'2 _1); False -> m' _1 v'2 _2 }) }

main = 
  let _3 = GHC.Enum.enumFromTo (0::Int) ((1000::Int) * (1000::Int)) in
  Criterion.Main.defaultMain (Criterion.Measurement.Types.bgroup (GHC.CString.unpackCString# "maxMaybe"#) ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "normal"#) $ Criterion.Measurement.Types.nf call'7 _3) : ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "manual"#) $ Criterion.Measurement.Types.nf call'4 _3) : [])) : [])

call'8 _4 = _4

lam'8 x'5 = \y'4 -> (lam'4 x'5 y'4)

call'6 _5 y'5 x'6 = case x'6 > y'5 of { True -> (case x'6 > _5 of { True -> x'6; False -> _5 }); False -> (case y'5 > _5 of { True -> y'5; False -> _5 }) }

lam'6 = \x'7 -> (lam'8 x'7)

call'2 flag_ds flag_ds_ds y'6 z'2 _fε z'3 = let
  π = case flag_ds_ds of { True -> z'3; False -> (case flag_ds of { True -> y'6; False -> (let (:) arg _ = _fε in arg) }) }
  t' = (call'2 False flag_ds undefined undefined (let (:) _ arg = _fε in arg) z'2)
  _fε' = case flag_ds of { True -> t'; False -> t' }
  ψ = case π > _fε' of { True -> π; False -> _fε' }
  in case flag_ds_ds of { True -> π; False -> (case flag_ds of { True -> ψ; False -> (case (let (:) _ arg = _fε in arg) of { (:) ρ ρ' -> ψ; [] -> π }) }) }

k = GHC.Enum.enumFromTo (0::Int) ((1000::Int) * (1000::Int))

lam'5 = \ds -> 
  let t'2 = (call'2 False False undefined undefined (let (:) _ arg = ds in arg) undefined) in
  case ds of { (:) ρ'2 ρ'3 -> Just (case ρ'3 of { (:) ρ'4 ρ'5 -> (case ρ'2 > t'2 of { True -> ρ'2; False -> t'2 }); [] -> ρ'2 }); [] -> Nothing }

max3_manual = lam'7

call'9 _6 = _6

lam' y'7 x'8 = \z'4 -> case x'8 > y'7 of { True -> (case x'8 > z'4 of { True -> x'8; False -> z'4 }); False -> (case y'7 > z'4 of { True -> y'7; False -> z'4 }) }

call'7 = map (lam'3 False True undefined)
