-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  14
-- Incl. one-shot:   0
-- Case reductions:  29
-- Field reductions: 30
-- Case commutings:  0
-- Total nodes: 124; Boxes: 47; Branches: 39
-- Apps: 8; Lams: 0

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module InterpTrivial (test3,test2,test1,test0) where

import Data.Tuple.Select
import GHC.Num
import GHC.Tuple
import GHC.Types

test0 = call

call' pgmtail' = sel2 (call'2 False False True False ((123::Int) + (1::Int)) (sel1 (call'3 (let (:) _ arg = pgmtail' in arg))) (sel2 (call'3 (let (:) _ arg = pgmtail' in arg))) (sel3 (call'3 (let (:) _ arg = pgmtail' in arg))))

call'4 flag_ds flag_ds_ds_ds flag_ds_ds_ds_ds_ds flag_ds_ds_ds_ds flag_ds_ds _0 ds'3 ds'4 = 
  let _cε = sel1 (call'4 False flag_ds_ds flag_ds_ds_ds_ds flag_ds_ds_ds flag_ds (_0 + (1::Int)) (sel1 (call'5 (let (:) _ arg = ds'3 in arg))) (sel2 (call'5 (let (:) _ arg = ds'3 in arg)))) in
  (,) (case flag_ds_ds_ds_ds_ds of { True -> _0; False -> (case flag_ds of { True -> _cε; False -> (case flag_ds_ds_ds of { True -> _cε; False -> (case flag_ds_ds of { True -> _cε; False -> (case flag_ds_ds_ds_ds of { True -> _cε; False -> (case ds'4 of { (:) ρ ρ' -> _cε; [] -> _0 }) }) }) }) }) }) _cε

call'6 = undefined

call'7 _fε = (,) _fε _fε

test3 = (call' undefined)

call'8 = undefined

call'9 flag_ds'_ds' flag_ds' flag_ds'_ds'_ds' x ds'5 ds'6 = 
  let _1 = (call'9 flag_ds' False flag_ds'_ds' x (sel1 (call'7 (let (:) _ arg = ds'6 in arg))) (sel2 (call'7 (let (:) _ arg = ds'6 in arg)))) + (1::Int) in
  case flag_ds' of { True -> _1; False -> (case flag_ds'_ds'_ds' of { True -> x; False -> (case flag_ds'_ds' of { True -> _1; False -> (case ds'5 of { (:) ρ'2 ρ'3 -> _1; [] -> x }) }) }) }

call'10 flag_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2 flag_ds'2_ds'2 flag_ds'2_ds'2_ds'2_ds'2 flag_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2 flag_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2 flag_ds'2_ds'2_ds'2_ds'2_ds'2 flag_ds'2_ds'2_ds'2 flag_ds'2 _fε' = 
  let _2 = (call'10 flag_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2 flag_ds'2 flag_ds'2_ds'2_ds'2 flag_ds'2_ds'2_ds'2_ds'2_ds'2 flag_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2 flag_ds'2_ds'2_ds'2_ds'2 flag_ds'2_ds'2 False (let (:) _ arg = _fε' in arg)) in
  (case flag_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2 of { True -> _2; False -> (case flag_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2 of { True -> (0::Int); False -> (case flag_ds'2_ds'2_ds'2_ds'2_ds'2_ds'2 of { True -> _2; False -> (case flag_ds'2 of { True -> _2; False -> (case flag_ds'2_ds'2_ds'2_ds'2 of { True -> _2; False -> (case flag_ds'2_ds'2_ds'2_ds'2_ds'2 of { True -> _2; False -> (case flag_ds'2_ds'2_ds'2 of { True -> _2; False -> (case flag_ds'2_ds'2 of { True -> _2; False -> (case (let (:) _ arg = _fε' in arg) of { (:) ρ'4 ρ'5 -> _2; [] -> (0::Int) }) }) }) }) }) }) }) }) }) + (1::Int)

test1 = (call'11 undefined)

call'3 _fε'2 = (,,) _fε'2 _fε'2 _fε'2

call'12 ds'7 = sel2 (call'4 True False False False False ((123::Int) + (1::Int)) (sel1 (call'5 (let (:) _ arg = ds'7 in arg))) (sel2 (call'5 (let (:) _ arg = ds'7 in arg))))

call'2 flag_pgmtail_pgmtail_pgmtail_pgmtail flag_pgmtail_pgmtail_pgmtail flag_pgmtail flag_pgmtail_pgmtail _3 pgmtail'2 pgmtail'3 pgmtail'4 = let
  _cε' = sel1 (call'2 flag_pgmtail_pgmtail_pgmtail flag_pgmtail_pgmtail False flag_pgmtail (_3 + (1::Int)) (sel1 (call'3 (let (:) _ arg = pgmtail'2 in arg))) (sel2 (call'3 (let (:) _ arg = pgmtail'2 in arg))) (sel3 (call'3 (let (:) _ arg = pgmtail'2 in arg))))
  _cε'2 = case flag_pgmtail_pgmtail_pgmtail of { True -> _cε'; False -> (case flag_pgmtail of { True -> _cε'; False -> (case flag_pgmtail_pgmtail of { True -> _cε'; False -> (case (let (:) arg _ = pgmtail'4 in arg) of { () -> _cε' }) }) }) }
  in (,) (case flag_pgmtail of { True -> _cε'2; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail of { True -> _3; False -> (case flag_pgmtail_pgmtail_pgmtail of { True -> _cε'2; False -> (case flag_pgmtail_pgmtail of { True -> _cε'2; False -> (case pgmtail'3 of { (:) ρ'6 ρ'7 -> _cε'2; [] -> _3 }) }) }) }) }) _cε'

call'5 _fε'3 = (,) _fε'3 _fε'3

call'11 ds'8 = (call'9 False True False (123::Int) (sel1 (call'7 (let (:) _ arg = ds'8 in arg))) (sel2 (call'7 (let (:) _ arg = ds'8 in arg)))) + (1::Int)

call = (call'10 False False False False False False False True (let (:) _ arg = undefined in arg)) + (1::Int)

test2 = (call'12 undefined)

call'13 = undefined
