-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  14
-- Incl. one-shot:   1
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 98; Boxes: 27; Branches: 21
-- Apps: 20; Lams: 3

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module IterContLocal (nats1_5,nats1,nats0) where

import Data.Tuple.Select
import GHC.List
import GHC.Num
import GHC.Types

lam flag_f flag_f_f f' f'2 f'3 = \state -> 
  let λ = sel2 (call'2 flag_f f'2 f') in
  case flag_f_f of { True -> (call state λ); False -> (case flag_f of { True -> (call' flag_f state (sel4 (call'2 flag_f f'2 f')) (sel3 (call'2 flag_f f'2 f')) (sel1 (call'2 flag_f f'2 f'))); False -> f'3 λ state }) }

call'3 = (call'4 (0::Int))

call state' k' = state' : k' (state' + (1::Int))

nats1_5 = GHC.List.take (5::Int) call'3

call'5 λ' = (,) undefined λ'

call' flag_f' state'2 f'4 f'5 f'6 = state'2 : (call'6 flag_f' (state'2 + (1::Int)) f'5 f'4 f'6)

nats0 = (call'7 (sel1 (call'5 lam')) (sel2 (call'5 lam')))

call'8 λ'2 = λ'2

call'9 state'3 = (call'4 state'3)

call'7 f'7 f'8 = (call' True (0::Int) (sel4 (call'2 True f'8 f'7)) (sel3 (call'2 True f'8 f'7)) (sel1 (call'2 True f'8 f'7)))

lam'2 flag_f'2 flag_k f'9 k'2 f'10 f'11 = \s -> 
  let _0 = s + (1::Int) in
  s : (case flag_k of { True -> (call'6 flag_f'2 _0 f'10 f'9 f'11); False -> k'2 _0 })

lam' = \k -> (lam'2 False False undefined k undefined undefined)

call'4 st = st : (call'4 (st + (1::Int)))

call'10 _1 = (call'4 _1)

call'6 flag_f'3 _2 f'12 f'13 f'14 = 
  let λ'3 = sel2 (call'2 False f'13 f'12) in
  case flag_f'3 of { True -> (call _2 λ'3); False -> f'14 λ'3 _2 }

nats1 = call'3

call'2 flag_f'4 f'15 f'16 = (,,,) f'16 (lam False flag_f'4 f'15 f'15 f'16) f'15 f'15
