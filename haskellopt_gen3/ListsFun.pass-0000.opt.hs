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
  _0 = from + (1::Int)
  _1 = from > to
  in (,,,) (case _1 of { True -> []; False -> from : sel1 (call'2 _0 to) }) _1 (sel2 (call'2 _0 to)) (sel3 (call'2 _0 to))

call'3 from' = (,,) (sel4 (call' from' (5::Int))) (sel2 (call' from' (5::Int))) (sel3 (call' from' (5::Int)))

call flag_ds _2 _cfε _3 = 
  let _4 = (call False undefined (case flag_ds of { True -> _3; False -> (let (:) _ arg = _cfε in arg) }) undefined) + (1::Int) in
  case flag_ds of { True -> (case _2 of { True -> (0::Int); False -> _4 }); False -> (case _cfε of { (:) ρ'2 ρ'3 -> _4; [] -> (0::Int) }) }

enumFromTo_mine = lam'

lam'2 from'2 = \to' -> sel1 (call' from'2 to')

call'4 = (0::Int)

lam' = \from'3 -> (lam'2 from'3)

call'2 _5 to'2 = let
  ψ = sel1 (call'2 (_5 + (1::Int)) to'2)
  _6 = _5 > to'2
  in (,,) (case _6 of { True -> []; False -> _5 : ψ }) ψ _6

test = (call'5 (sel3 (call'3 (0::Int))) (sel2 (call'3 (0::Int))) (sel1 (call'3 (0::Int))))

call'5 _7 _8 _9 = case _8 of { True -> (0::Int); False -> (call True _9 (let (:) _ arg = undefined in arg) _7) + (1::Int) }
