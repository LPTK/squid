-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  12
-- Incl. one-shot:   0
-- Case reductions:  130
-- Field reductions: 210
-- Case commutings:  0
-- Total nodes: 375; Boxes: 118; Branches: 156
-- Apps: 42; Lams: 1

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main (main,k,src) where

import Criterion.Main
import Criterion.Measurement.Types
import GHC.Base
import GHC.CString
import GHC.Classes
import GHC.Maybe
import GHC.Num
import GHC.Types

main = Criterion.Main.defaultMain (Criterion.Measurement.Types.bgroup (GHC.CString.unpackCString# "interp"#) ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "normal"#) $ Criterion.Measurement.Types.whnf (\x -> let
  _0 = x + (1::Int)
  rec x' _1 _2 = 
        let _3 = x' + (1::Int) in
        (case _3 < (0::Int) of { True -> (rec _3 _1 _1); False -> (0::Int) }) + _2
  in (case _0 < (0::Int) of { True -> (rec _0 (2::Int) (2::Int)); False -> (0::Int) }) + (2::Int)) (negate ((1000::Int) * (100::Int)))) : []) : [])

k = negate ((1000::Int) * (100::Int))

src = Nothing : (Just (Just (2::Int)) : (Just Nothing : []))