-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   Simplifier: Max iterations = 4
--               SimplMode {Phase = 0 [Non-opt simplification],
--                          inline,
--                          no rules,
--                          eta-expand,
--                          case-of-case}
-- Total nodes: 31; Boxes: 10; Branches: 5
-- Apps: 4; Lams: 3; Unreduced Redexes: 1

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main (main,rec0,rec0_0,rec0') where

import GHC.Base
import GHC.TopHandler

main = (GHC.TopHandler.runMainIO (GHC.Base.return ()))

rec0 = (\f -> (_0(# f #)))

_0(# f' #) = (f' (\x -> ((_0(# f' #)) x)))

rec0' = (_0(# GHC.Base.id #))

rec0_0 = GHC.Base.id
