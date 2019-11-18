-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  12
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 221; Boxes: 64; Branches: 46
-- Apps: 45; Lams: 12

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module IterCont (nats1_5,nats1,loop1,nats0_5,nats0,loop0) where

import Data.Tuple.Select
import GHC.List
import GHC.Num
import GHC.Types

lam = \f' -> f' (sel1 (call False f' f'))

loop0 = lam

call' f'2 = 
  let λ = (lam' True f'2 λ) in
  sel1 (call'2 True (0::Int) f'2 λ)

call'3 _0 k' = _0 : k' (_0 + (1::Int))

lam'2 flag_f f'3 = \state -> 
  let λ' = (lam' flag_f f'3 λ') in
  sel2 (call'2 flag_f state f'3 λ')

loop1 = lam'3

call'4 λ'2 = sel2 (call True λ'2 undefined)

nats1 = (call' undefined)

lam'4 = \k -> (lam'5 False k undefined)

call'5 _1 = (,) _1 (lam'5 False _1 undefined)

nats0 = (call'6 (call'4 lam'4))

call'6 k'2 = (0::Int) : (call'3 ((0::Int) + (1::Int)) k'2)

call'2 flag_f' state' f'4 λ'3 = 
  let ret = (call'7 flag_f' state' λ'3 f'4) in
  (,) ret (case flag_f' of { True -> ret; False -> f'4 λ'3 state' })

lam'3 = \f -> (lam'2 False f)

call'7 flag_f'2 st λ'4 f'5 = st : (call'8 flag_f'2 (st + (1::Int)) f'5 λ'4)

lam' flag_f'3 f'6 λ'5 = \st' -> case flag_f'3 of { True -> (call'7 flag_f'3 st' λ'5 f'6); False -> f'6 λ'5 st' }

lam'5 flag_f'_k k'3 k'4 = \s -> 
  let _2 = s + (1::Int) in
  s : (case flag_f'_k of { True -> (call'3 _2 k'4); False -> k'3 _2 })

nats1_5 = GHC.List.take (5::Int) (call' undefined)

call'8 flag_f'4 _3 f'7 λ'6 = case flag_f'4 of { True -> (call'7 flag_f'4 _3 λ'6 f'7); False -> f'7 λ'6 _3 }

call flag_f' f'8 f'9 = let
  _4 = sel1 (call False f'8 f'8)
  ret' = (call'5 _4)
  in (,) (case flag_f' of { True -> sel2 ret'; False -> f'9 _4 }) (sel1 ret')

nats0_5 = GHC.List.take (5::Int) (call'6 (call'4 lam'4))

call'9 = undefined
