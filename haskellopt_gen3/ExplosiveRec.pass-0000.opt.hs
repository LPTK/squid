-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  3
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 21; Boxes: 4; Branches: 3
-- Apps: 6; Lams: 1

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module ExplosiveRec (a0) where

import GHC.Base
import GHC.Num
import GHC.Types

call = (2::Int) : (call' ++ (call'2 ++ call))

lam = \x -> x : (call' ++ (call'2 ++ call))

call'2 = (1::Int) : (call' ++ (call'2 ++ call))

a0 = lam

call' = (0::Int) : (call' ++ (call'2 ++ call))
