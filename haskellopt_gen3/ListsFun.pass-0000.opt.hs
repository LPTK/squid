-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  6
-- Incl. one-shot:   0
-- Case reductions:  4
-- Field reductions: 4
-- Case commutings:  5
-- Total nodes: 133; Boxes: 34; Branches: 21
-- Apps: 22; Lams: 3

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module ListsFun (test,enumFromTo_mine,length_mine) where

import Data.Tuple.Select
import GHC.Classes
import GHC.Num
import GHC.Types

length_mine = lam

lam = \ds -> case ds of { (:) ρ ρ' -> (call False undefined ρ' undefined) + (1::Int); [] -> (0::Int) }

call' from to = let
  ret = (call'2 (from + (1::Int)) to)
  _0 = from > to
  in (,,,) (case _0 of { True -> []; False -> from : sel1 ret }) _0 (sel2 ret) (sel3 ret)

call'3 from' = 
  let ret' = (call' from' (5::Int)) in
  (,,) (sel4 ret') (sel2 ret') (sel3 ret')

call flag_ds _1 _cfε _2 = 
  let _3 = (call False undefined (case flag_ds of { True -> _2; False -> (let (:) _ arg = _cfε in arg) }) undefined) + (1::Int) in
  case flag_ds of { True -> (case _1 of { True -> (0::Int); False -> _3 }); False -> (case _cfε of { (:) ρ'2 ρ'3 -> _3; [] -> (0::Int) }) }

enumFromTo_mine = lam'

lam'2 from'2 = \to' -> sel1 (call' from'2 to')

call'4 = (0::Int)

lam' = \from'3 -> (lam'2 from'3)

call'2 _4 to'2 = let
  ψ = sel1 (call'2 (_4 + (1::Int)) to'2)
  _5 = _4 > to'2
  in (,,) (case _5 of { True -> []; False -> _4 : ψ }) ψ _5

test = 
  let ret'2 = (call'3 (0::Int)) in
  (call'5 (sel3 ret'2) (sel2 ret'2) (sel1 ret'2))

call'5 _6 _7 _8 = case _7 of { True -> (0::Int); False -> (call True _8 (let (:) _ arg = undefined in arg) _6) + (1::Int) }
