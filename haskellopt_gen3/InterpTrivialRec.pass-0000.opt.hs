-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  7
-- Incl. one-shot:   0
-- Case reductions:  39
-- Field reductions: 20
-- Case commutings:  0
-- Total nodes: 734; Boxes: 200; Branches: 330
-- Apps: 26; Lams: 1

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module InterpTrivialRec (test3_10,test3,test2_10,test2,test1,test0,exec) where

import Data.Tuple.Select
import GHC.List
import GHC.Tuple
import GHC.Types

test3 = call

test1 = call'

call'2 flag_pgm flag_pgm' flag_ds_ds_pgm flag_pgm_ds flag_pgm_ds' flag_ds_pgm_ds flag_ds_ds_ds_pgm flag_ds_pgm_ds' flag_ds_ds_ds_pgm' flag_pgm_ds'2 flag_ds_ds_pgm' flag_pgm_ds'3 flag_pgm_ds'4 flag_pgm_ds'5 flag_pgm'2 flag_pgm'3 _fε pgm' = let
  _cε = (call'2 flag_pgm flag_pgm' flag_pgm_ds'5 False False flag_pgm_ds flag_ds_ds_pgm' flag_pgm_ds'2 flag_ds_pgm_ds' False flag_pgm_ds'4 False False False flag_pgm'2 flag_pgm'3 (let (:) _ arg = _fε in arg) pgm')
  _cε' = (call'3 flag_pgm flag_pgm' flag_pgm'2 flag_pgm'3 pgm' pgm')
  in (case flag_ds_ds_ds_pgm' of { True -> False; False -> (case flag_ds_pgm_ds' of { True -> True; False -> (case flag_pgm_ds'2 of { True -> True; False -> (case flag_ds_ds_ds_pgm of { True -> False; False -> (case flag_ds_ds_pgm' of { True -> True; False -> (case flag_ds_pgm_ds of { True -> False; False -> (case flag_pgm_ds'3 of { True -> False; False -> (case flag_pgm_ds' of { True -> False; False -> (case flag_pgm_ds'4 of { True -> True; False -> (case flag_pgm_ds of { True -> True; False -> (case flag_pgm_ds'5 of { True -> True; False -> (case flag_ds_ds_pgm of { True -> False; False -> (case _fε of { (:) ρ ρ' -> True; [] -> False }) }) }) }) }) }) }) }) }) }) }) }) }) : (case flag_ds_ds_ds_pgm' of { True -> _cε'; False -> (case flag_ds_pgm_ds' of { True -> _cε; False -> (case flag_pgm_ds'2 of { True -> _cε; False -> (case flag_ds_pgm_ds of { True -> _cε'; False -> (case flag_ds_ds_ds_pgm of { True -> _cε'; False -> (case flag_ds_ds_pgm' of { True -> _cε; False -> (case flag_pgm_ds'3 of { True -> _cε'; False -> (case flag_pgm_ds' of { True -> _cε'; False -> (case flag_pgm_ds'4 of { True -> _cε; False -> (case flag_pgm_ds of { True -> _cε; False -> (case flag_pgm_ds'5 of { True -> _cε; False -> (case flag_ds_ds_pgm of { True -> _cε'; False -> (case _fε of { (:) ρ'2 ρ'3 -> _cε; [] -> _cε' }) }) }) }) }) }) }) }) }) }) }) }) })

call' = sel2 (call'4 False True False False undefined undefined)

lam = \pgm -> sel2 (call'4 False False False False pgm pgm)

exec = lam

call'5 = sel2 (call'4 False False False True undefined undefined)

test3_10 = GHC.List.take (10::Int) call

test2 = call'5

call'4 flag_pgm'4 flag_pgm'5 flag_pgm'6 flag_pgm'7 pgm'2 pgm'3 = let
  _cε'2 = (call'2 flag_pgm'6 flag_pgm'7 False flag_pgm'7 False False False False False flag_pgm'6 False flag_pgm'5 False False flag_pgm'5 flag_pgm'4 (let (:) _ arg = pgm'2 in arg) pgm'3)
  _cε'3 = (call'3 flag_pgm'6 flag_pgm'7 flag_pgm'5 flag_pgm'4 pgm'3 pgm'3)
  in (,) (True : _cε'2) ((case flag_pgm'5 of { True -> True; False -> (case flag_pgm'4 of { True -> False; False -> (case flag_pgm'7 of { True -> True; False -> (case pgm'2 of { (:) ρ'4 ρ'5 -> True; [] -> False }) }) }) }) : (case flag_pgm'5 of { True -> _cε'2; False -> (case flag_pgm'4 of { True -> _cε'3; False -> (case flag_pgm'7 of { True -> _cε'2; False -> (case pgm'2 of { (:) ρ'6 ρ'7 -> _cε'2; [] -> _cε'3 }) }) }) }))

test2_10 = GHC.List.take (10::Int) call'5

test0 = call'6

call'3 flag_pgm'8 flag_pgm'9 flag_pgm'10 flag_pgm'11 pgm'4 pgm'5 = let
  _cε'4 = (call'3 flag_pgm'8 flag_pgm'9 flag_pgm'10 flag_pgm'11 pgm'5 pgm'5)
  _cε'5 = (call'2 flag_pgm'8 flag_pgm'9 False False flag_pgm'10 False False False False False False False flag_pgm'8 flag_pgm'9 flag_pgm'10 flag_pgm'11 (let (:) _ arg = pgm'4 in arg) pgm'5)
  in (case flag_pgm'10 of { True -> True; False -> (case flag_pgm'9 of { True -> True; False -> (case flag_pgm'11 of { True -> False; False -> (case flag_pgm'8 of { True -> True; False -> (case pgm'4 of { (:) ρ'8 ρ'9 -> True; [] -> False }) }) }) }) }) : (case flag_pgm'8 of { True -> _cε'5; False -> (case flag_pgm'10 of { True -> _cε'5; False -> (case flag_pgm'9 of { True -> _cε'5; False -> (case flag_pgm'11 of { True -> _cε'4; False -> (case pgm'4 of { (:) ρ'10 ρ'11 -> _cε'5; [] -> _cε'4 }) }) }) }) })

call'6 = sel2 (call'4 True False False False undefined undefined)

call = sel1 (call'4 False False True False undefined undefined)
