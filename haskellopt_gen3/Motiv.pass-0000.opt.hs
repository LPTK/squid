-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  7
-- Incl. one-shot:   0
-- Case reductions:  9
-- Field reductions: 4
-- Case commutings:  1
-- Total nodes: 195; Boxes: 34; Branches: 43
-- Apps: 18; Lams: 6

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Motiv (pgrm,f,e1,e2,e3,e0,isJust) where

import GHC.Maybe
import GHC.Num
import GHC.Tuple
import GHC.Types

lam = \c -> case c of { True -> (1::Int); False -> (0::Int) }

f = lam'

e3 = lam

call ψ π = π * ψ

call' ψ' = ψ' + (1::Int)

lam' = \x -> 
  let ret = (call'2 False False x) in
  case x of { Just ρ -> ρ * ret; Nothing -> (call' ret) }

isJust = lam'2

lam'3 = \z -> z + (1::Int)

pgrm = call'3 + call'4

e2 = lam'3

lam'2 = \ds -> case ds of { Just ρ' -> True; Nothing -> False }

lam'4 = \a -> a

e1 = lam'5

e0 = lam'4

call'2 flag_x flag_x' x' = case flag_x' of { True -> (1::Int); False -> (case flag_x of { True -> (0::Int); False -> (case x' of { Just ρ'2 -> (1::Int); Nothing -> (0::Int) }) }) }

lam'5 = \ds' -> case ds' of { (,) ρ'3 ρ'4 -> ρ'3 * ρ'4 }

call'4 = (call' (call'2 True False undefined))

call'3 = (2::Int) * (call'2 False True undefined)
