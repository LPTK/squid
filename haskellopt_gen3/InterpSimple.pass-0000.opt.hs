-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  6
-- Incl. one-shot:   0
-- Case reductions:  46
-- Field reductions: 62
-- Case commutings:  0
-- Total nodes: 113; Boxes: 44; Branches: 54
-- Apps: 4; Lams: 0

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module InterpSimple (test) where

import Data.Tuple.Select
import GHC.Num
import GHC.Types

call _fε = (,,) _fε _fε _fε

call' flag_pgmtail_pgmtail_pgmtail_pgmtail flag_pgmtail_pgmtail_pgmtail_pgmtail' flag_pgmtail_pgmtail_pgmtail_pgmtail'2 flag_pgmtail_pgmtail_pgmtail_pgmtail'3 flag_pgmtail_pgmtail_pgmtail flag_pgmtail_pgmtail_pgmtail' flag_pgmtail flag_pgmtail_pgmtail flag_pgmtail_pgmtail' flag_pgmtail_pgmtail_pgmtail'2 flag_pgmtail_pgmtail_pgmtail'3 flag_pgmtail_pgmtail_pgmtail_pgmtail'4 flag_pgmtail_pgmtail_pgmtail_pgmtail'5 flag_pgmtail_pgmtail_pgmtail_pgmtail'6 flag_pgmtail_pgmtail_pgmtail_pgmtail'7 _0 pgmtail' pgmtail'2 pgmtail'3 = let
  _cε = (call' flag_pgmtail_pgmtail_pgmtail' False flag_pgmtail_pgmtail_pgmtail'3 flag_pgmtail_pgmtail_pgmtail'2 flag_pgmtail_pgmtail' False False False flag_pgmtail flag_pgmtail_pgmtail False flag_pgmtail_pgmtail_pgmtail False False False (_0 * (2::Int)) (sel3 (call (let (:) _ arg = pgmtail' in arg))) (sel2 (call (let (:) _ arg = pgmtail' in arg))) (sel1 (call (let (:) _ arg = pgmtail' in arg))))
  _cε'2 = sel2 (call'2 flag_pgmtail_pgmtail_pgmtail False False flag_pgmtail_pgmtail_pgmtail'2 False flag_pgmtail_pgmtail' flag_pgmtail False flag_pgmtail_pgmtail_pgmtail' False False flag_pgmtail_pgmtail False flag_pgmtail_pgmtail_pgmtail'3 False (_0 + (1::Int)) (sel3 (call'3 (let (:) _ arg = pgmtail' in arg))) (sel2 (call'3 (let (:) _ arg = pgmtail' in arg))) (sel1 (call'3 (let (:) _ arg = pgmtail' in arg))))
  _cε' = case flag_pgmtail_pgmtail' of { True -> _cε; False -> (case flag_pgmtail_pgmtail_pgmtail'2 of { True -> _cε'2; False -> (case flag_pgmtail_pgmtail_pgmtail of { True -> _cε'2; False -> (case flag_pgmtail_pgmtail of { True -> _cε; False -> (case flag_pgmtail of { True -> _cε; False -> (case flag_pgmtail_pgmtail_pgmtail' of { True -> _cε'2; False -> (case flag_pgmtail_pgmtail_pgmtail'3 of { True -> _cε'2; False -> (case (let (:) arg _ = pgmtail'3 in arg) of { True -> _cε'2; False -> _cε }) }) }) }) }) }) }) }
  in case flag_pgmtail_pgmtail of { True -> _cε'; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'4 of { True -> _0; False -> (case flag_pgmtail_pgmtail_pgmtail of { True -> _cε'; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'3 of { True -> _0; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'5 of { True -> _0; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'2 of { True -> _0; False -> (case flag_pgmtail of { True -> _cε'; False -> (case flag_pgmtail_pgmtail' of { True -> _cε'; False -> (case flag_pgmtail_pgmtail_pgmtail'3 of { True -> _cε'; False -> (case flag_pgmtail_pgmtail_pgmtail'2 of { True -> _cε'; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'6 of { True -> _0; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail' of { True -> _0; False -> (case flag_pgmtail_pgmtail_pgmtail' of { True -> _cε'; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'7 of { True -> _0; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail of { True -> _0; False -> (case pgmtail'2 of { (:) ρ ρ' -> _cε'; [] -> _0 }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }

call'2 flag_pgmtail_pgmtail_pgmtail_pgmtail'8 flag_pgmtail_pgmtail_pgmtail_pgmtail'9 flag_pgmtail_pgmtail_pgmtail_pgmtail'10 flag_pgmtail_pgmtail_pgmtail_pgmtail'11 flag_pgmtail_pgmtail_pgmtail'4 flag_pgmtail_pgmtail_pgmtail'5 flag_pgmtail_pgmtail'2 flag_pgmtail' flag_pgmtail_pgmtail_pgmtail_pgmtail'12 flag_pgmtail_pgmtail'3 flag_pgmtail_pgmtail_pgmtail'6 flag_pgmtail_pgmtail_pgmtail'7 flag_pgmtail_pgmtail_pgmtail_pgmtail'13 flag_pgmtail_pgmtail_pgmtail_pgmtail'14 flag_pgmtail_pgmtail_pgmtail_pgmtail'15 _1 pgmtail'4 pgmtail'5 pgmtail'6 = let
  _cε'3 = (call' False flag_pgmtail_pgmtail_pgmtail'4 False False False flag_pgmtail_pgmtail'2 False flag_pgmtail' False False flag_pgmtail_pgmtail'3 False flag_pgmtail_pgmtail_pgmtail'6 flag_pgmtail_pgmtail_pgmtail'7 flag_pgmtail_pgmtail_pgmtail'5 (_1 * (2::Int)) (sel3 (call (let (:) _ arg = pgmtail'4 in arg))) (sel2 (call (let (:) _ arg = pgmtail'4 in arg))) (sel1 (call (let (:) _ arg = pgmtail'4 in arg))))
  _cε'5 = sel2 (call'2 False flag_pgmtail_pgmtail_pgmtail'6 flag_pgmtail_pgmtail_pgmtail'5 False flag_pgmtail_pgmtail'2 False False False False flag_pgmtail' flag_pgmtail_pgmtail'3 False flag_pgmtail_pgmtail_pgmtail'4 False flag_pgmtail_pgmtail_pgmtail'7 (_1 + (1::Int)) (sel3 (call'3 (let (:) _ arg = pgmtail'4 in arg))) (sel2 (call'3 (let (:) _ arg = pgmtail'4 in arg))) (sel1 (call'3 (let (:) _ arg = pgmtail'4 in arg))))
  _cε'4 = case flag_pgmtail' of { True -> _cε'3; False -> (case flag_pgmtail_pgmtail'3 of { True -> _cε'3; False -> (case flag_pgmtail_pgmtail_pgmtail'5 of { True -> _cε'5; False -> (case flag_pgmtail_pgmtail_pgmtail'6 of { True -> _cε'5; False -> (case flag_pgmtail_pgmtail'2 of { True -> _cε'3; False -> (case flag_pgmtail_pgmtail_pgmtail'4 of { True -> _cε'5; False -> (case flag_pgmtail_pgmtail_pgmtail'7 of { True -> _cε'5; False -> (case (let (:) arg _ = pgmtail'6 in arg) of { True -> _cε'5; False -> _cε'3 }) }) }) }) }) }) }) }
  in (,) _cε'3 (case flag_pgmtail_pgmtail_pgmtail_pgmtail'12 of { True -> _1; False -> (case flag_pgmtail_pgmtail'3 of { True -> _cε'4; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'11 of { True -> _1; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'13 of { True -> _1; False -> (case flag_pgmtail_pgmtail_pgmtail'5 of { True -> _cε'4; False -> (case flag_pgmtail_pgmtail_pgmtail'7 of { True -> _cε'4; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'10 of { True -> _1; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'14 of { True -> _1; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'9 of { True -> _1; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'15 of { True -> _1; False -> (case flag_pgmtail_pgmtail'2 of { True -> _cε'4; False -> (case flag_pgmtail_pgmtail_pgmtail_pgmtail'8 of { True -> _1; False -> (case flag_pgmtail' of { True -> _cε'4; False -> (case flag_pgmtail_pgmtail_pgmtail'4 of { True -> _cε'4; False -> (case flag_pgmtail_pgmtail_pgmtail'6 of { True -> _cε'4; False -> (case pgmtail'5 of { (:) ρ'2 ρ'3 -> _cε'4; [] -> _1 }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })

call'3 _fε' = (,,) _fε' _fε' _fε'

call'4 pgmtail'7 = sel1 (call'2 False False False False False False False True False False False False False False False ((123::Int) + (1::Int)) (sel3 (call'3 (let (:) _ arg = pgmtail'7 in arg))) (sel2 (call'3 (let (:) _ arg = pgmtail'7 in arg))) (sel1 (call'3 (let (:) _ arg = pgmtail'7 in arg))))

test = (call'4 undefined)

call'5 = undefined
