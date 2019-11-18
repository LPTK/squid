-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  15
-- Incl. one-shot:   1
-- Case reductions:  16
-- Field reductions: 28
-- Case commutings:  0
-- Total nodes: 160; Boxes: 40; Branches: 32
-- Apps: 34; Lams: 1

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main (main,k,src) where

import Data.Tuple.Select
import Criterion.Main
import Criterion.Measurement.Types
import GHC.Base
import GHC.CString
import GHC.Classes
import GHC.Num
import GHC.Tuple
import GHC.Types

src = (0::Int) : ((2::Int) : [])

call _0 = _0

call' ds' x = let
  ret = (call'2 (let (:) _ arg = ds' in arg))
  ret' = (call'7 (let (:) _ arg = ds' in arg))
  in case (0::Int) == (0::Int) of { True -> (call'3 False False True False False False ((0::Int) + (1::Int)) (sel3 ret') (sel1 ret') (sel2 ret') (x + (1::Int))); False -> (case (0::Int) == (2::Int) of { True -> (case x > (0::Int) of { True -> (call'4 (0::Int) undefined x); False -> (call'5 False False True False False False (0::Int) (sel3 ret) (sel1 ret) (sel2 ret) x) }); False -> call'6 }) }

call'7 _fε = (,,) _fε _fε _fε

call'8 = call'6

call'5 flag_ds_ds flag_ds_ds' flag_ds flag_ds_ds'2 flag_ds' flag_ds_ds'3 y ds'2 ds'3 ds'4 x' = let
  _fε' = case flag_ds of { True -> (2::Int); False -> (case flag_ds' of { True -> (2::Int); False -> (let (:) arg _ = ds'4 in arg) }) }
  ret' = (call'7 (let (:) _ arg = ds'3 in arg))
  ret = (call'2 (let (:) _ arg = ds'3 in arg))
  ψ = case _fε' == (0::Int) of { True -> (call'3 flag_ds' False False False False flag_ds (y + (1::Int)) (sel3 ret') (sel1 ret') (sel2 ret') (x' + (1::Int))); False -> (case _fε' == (2::Int) of { True -> (case x' > (0::Int) of { True -> (call'4 y undefined x'); False -> (call'5 False flag_ds' False flag_ds False False y (sel3 ret) (sel1 ret) (sel2 ret) x') }); False -> call'6 }) }
  in case flag_ds_ds'2 of { True -> y; False -> (case flag_ds of { True -> ψ; False -> (case flag_ds_ds' of { True -> y; False -> (case flag_ds_ds'3 of { True -> y; False -> (case flag_ds' of { True -> ψ; False -> (case flag_ds_ds of { True -> y; False -> (case ds'2 of { (:) ρ ρ' -> ψ; [] -> y }) }) }) }) }) }) }

call'2 _fε'2 = (,,) _fε'2 _fε'2 _fε'2

call'4 y' ds'5 x'2 = let
  ret' = (call'7 (let (:) _ arg = ds'5 in arg))
  ret = (call'2 (let (:) _ arg = ds'5 in arg))
  in case (0::Int) == (0::Int) of { True -> (call'3 False False False False True False (y' + (1::Int)) (sel3 ret') (sel1 ret') (sel2 ret') (x'2 + (1::Int))); False -> (case (0::Int) == (2::Int) of { True -> (case x'2 > (0::Int) of { True -> (call'4 y' undefined x'2); False -> (call'5 False False False False True False y' (sel3 ret) (sel1 ret) (sel2 ret) x'2) }); False -> call'6 }) }

k = negate ((1000::Int) * (100::Int))

call'9 x'3 = x'3

call'10 x'4 = x'4

call'11 = undefined

call'12 x'5 = x'5

lam = \x'6 -> (call' undefined x'6)

call'3 flag_ds_ds'4 flag_ds_ds'5 flag_ds'2 flag_ds_ds'6 flag_ds'3 flag_ds_ds'7 _1 ds'6 ds'7 ds'8 x'7 = let
  _fε'3 = case flag_ds'2 of { True -> (2::Int); False -> (case flag_ds'3 of { True -> (2::Int); False -> (let (:) arg _ = ds'8 in arg) }) }
  ret = (call'2 (let (:) _ arg = ds'7 in arg))
  ret' = (call'7 (let (:) _ arg = ds'7 in arg))
  ψ' = case _fε'3 == (0::Int) of { True -> (call'3 False flag_ds'2 False flag_ds'3 False False (_1 + (1::Int)) (sel3 ret') (sel1 ret') (sel2 ret') (x'7 + (1::Int))); False -> (case _fε'3 == (2::Int) of { True -> (case x'7 > (0::Int) of { True -> (call'4 _1 undefined x'7); False -> (call'5 flag_ds'2 False False False False flag_ds'3 _1 (sel3 ret) (sel1 ret) (sel2 ret) x'7) }); False -> call'6 }) }
  in case flag_ds_ds'6 of { True -> _1; False -> (case flag_ds'2 of { True -> ψ'; False -> (case flag_ds_ds'5 of { True -> _1; False -> (case flag_ds'3 of { True -> ψ'; False -> (case flag_ds_ds'7 of { True -> _1; False -> (case flag_ds_ds'4 of { True -> _1; False -> (case ds'6 of { (:) ρ'2 ρ'3 -> ψ'; [] -> _1 }) }) }) }) }) }) }

call'13 = undefined

call'6 = call'6

main = Criterion.Main.defaultMain (Criterion.Measurement.Types.bgroup (GHC.CString.unpackCString# "interp"#) ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "normal"#) $ Criterion.Measurement.Types.whnf lam (negate ((1000::Int) * (100::Int)))) : []) : [])

call'14 = lam
