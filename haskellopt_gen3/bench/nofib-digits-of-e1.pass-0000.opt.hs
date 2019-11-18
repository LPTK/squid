-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  13
-- Incl. one-shot:   0
-- Case reductions:  3
-- Field reductions: 22
-- Case commutings:  0
-- Total nodes: 936; Boxes: 152; Branches: 149
-- Apps: 312; Lams: 10

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main (main,e,takeDigits,ratTrans,eContFrac,hash) where

import Data.Tuple.Select
import Control.Exception.Base
import Criterion.Main
import Criterion.Measurement.Types
import Data.Foldable
import GHC.Base
import GHC.CString
import GHC.Classes
import GHC.Num
import GHC.Real
import GHC.Show
import GHC.Tuple
import GHC.Types

lam = \n -> (lam' n)

lam'2 flag_ds' flag_ds'' flag_ds''2 _0 π _1 ds'2 _2 π' π'2 _3 = \xs -> let
  π'3 = case flag_ds'' of { True -> (0::Int); False -> (case flag_ds''2 of { True -> π'; False -> (case flag_ds' of { True -> _0; False -> (let (,,,) _ _ arg _ = ds'2 in arg) }) }) }
  π'4 = case flag_ds'' of { True -> (1::Int); False -> (case flag_ds''2 of { True -> _1; False -> (case flag_ds' of { True -> _3; False -> (let (,,,) _ _ _ arg = ds'2 in arg) }) }) }
  π'6 = case flag_ds'' of { True -> (0::Int); False -> (case flag_ds''2 of { True -> _2; False -> (case flag_ds' of { True -> π'; False -> (let (,,,) _ arg _ _ = ds'2 in arg) }) }) }
  π'5 = case flag_ds'' of { True -> (10::Int); False -> (case flag_ds''2 of { True -> π; False -> (case flag_ds' of { True -> π'2; False -> (let (,,,) arg _ _ _ = ds'2 in arg) }) }) }
  _4 = div π'6 π'4
  ψ = case ((signum π'3 == signum π'4) || (abs π'3 < abs π'4)) && ((((π'3 + π'4) * _4) <= (π'5 + π'6)) && ((((π'3 + π'4) * _4) + (π'3 + π'4)) > (π'5 + π'6))) of { True -> _4 : (call xs (π'5 - (_4 * π'3)) π'4 π'3 (π'6 - (_4 * π'4))); False -> (call' False False False undefined (let (:) _ arg = xs in arg) undefined (let (:) _ arg = xs in arg) undefined undefined undefined undefined π'6 (π'3 + ((let (:) arg _ = xs in arg) * π'4)) (π'5 + ((let (:) arg _ = xs in arg) * π'6)) π'4) }
  in case flag_ds'' of { True -> ψ; False -> (case flag_ds''2 of { True -> ψ; False -> (case flag_ds' of { True -> ψ; False -> (case ds'2 of { (,,,) ρ ρ' ρ'2 ρ'3 -> ψ }) }) }) }

eContFrac = (2::Int) : sel1 call'2

call'3 _5 _6 _7 _8 _9 n' = case n' == (0::Int) of { True -> []; False -> (2::Int) : (call'4 (call'5 True _9 (let (:) _ arg = undefined in arg) _8 _7 _5 _6) (n' - (1::Int))) }

call' flag_xs_ds_xs flag_ds_xs flag_xs_xs_xs_ds _10 π'7 _11 _fε _12 _13 _14 _15 π'8 _16 _17 π'9 = let
  _18 = div _17 _16
  _fε'2 = case flag_ds_xs of { True -> (2::Int); False -> (case flag_xs_xs_xs_ds of { True -> (1::Int); False -> (case flag_xs_ds_xs of { True -> (1::Int); False -> (let (:) arg _ = _fε in arg) }) }) }
  _fε' = case flag_xs_xs_xs_ds of { True -> _10; False -> (let (:) _ arg = _fε in arg) }
  in case ((signum π'9 == signum _16) || (abs π'9 < abs _16)) && ((((π'9 + _16) * _18) <= (π'8 + _17)) && ((((π'9 + _16) * _18) + (π'9 + _16)) > (π'8 + _17))) of { True -> _18 : (call π'7 (π'8 - (_18 * π'9)) _16 π'9 (_17 - (_18 * _16))); False -> (call' flag_ds_xs False flag_xs_ds_xs _12 (case flag_ds_xs of { True -> _11; False -> (case flag_xs_ds_xs of { True -> _13; False -> _fε' }) }) undefined _fε' _14 _15 undefined undefined _17 (π'9 + (_fε'2 * _16)) (π'8 + (_fε'2 * _17)) _16) }

call'6 _19 = _19

lam'3 = \acc -> (lam'4 acc)

e = lam'5

main = Criterion.Main.defaultMain ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "main"#) $ Criterion.Measurement.Types.whnf lam'6 (420::Int)) : [])

lam'5 = \n'2 -> (call'3 (sel5 call'2) (sel3 call'2) (sel1 call'2) (sel2 call'2) (sel4 call'2) n'2)

call'7 _20 = 
  let _21 = _20 : ((1::Int) : sel1 (call'7 (_20 + (2::Int)))) in
  (,) ((1::Int) : _21) _21

ratTrans = lam'7

lam'7 = \ds' -> (lam'2 False False False undefined undefined undefined ds' undefined undefined undefined undefined)

call xs' _22 π'10 π'11 _23 = 
  let _24 = div π'10 _23 in
  case ((signum _22 == signum _23) || (abs _22 < abs _23)) && ((((_22 + _23) * _24) <= (π'11 + π'10)) && ((((_22 + _23) * _24) + (_22 + _23)) > (π'11 + π'10))) of { True -> _24 : (call xs' (π'11 - (_24 * _22)) _23 _22 (π'10 - (_24 * _23))); False -> (call' False False False undefined (let (:) _ arg = xs' in arg) undefined (let (:) _ arg = xs' in arg) undefined undefined undefined undefined π'10 (_22 + ((let (:) arg _ = xs' in arg) * _23)) (π'11 + ((let (:) arg _ = xs' in arg) * π'10)) _23) }

call'8 n'3 = n'3

lam'6 = \d -> Data.Foldable.foldl' lam'3 (0::Int) (GHC.Show.show (call'9 d))

hash = Data.Foldable.foldl' lam'3 (0::Int)

call'4 ψ' n'4 = case n'4 == (0::Int) of { True -> []; False -> (let (:) arg _ = ψ' in arg) : (call'4 (call'5 False undefined (let (:) _ arg = ψ' in arg) undefined (let (:) _ arg = ψ' in arg) undefined undefined) (n'4 - (1::Int))) }

call'5 flag_ds _25 _fε'3 _26 π'12 _27 _28 = let
  _29 = div (0::Int) (1::Int)
  π'13 = case flag_ds of { True -> (1::Int); False -> (let (:) arg _ = _fε'3 in arg) }
  in case ((signum (0::Int) == signum (1::Int)) || (abs (0::Int) < abs (1::Int))) && (((((0::Int) + (1::Int)) * _29) <= ((10::Int) + (0::Int))) && (((((0::Int) + (1::Int)) * _29) + ((0::Int) + (1::Int))) > ((10::Int) + (0::Int)))) of { True -> _29 : (call π'12 ((10::Int) - (_29 * (0::Int))) (1::Int) (0::Int) ((0::Int) - (_29 * (1::Int)))); False -> (call' False flag_ds False undefined (case flag_ds of { True -> _27; False -> (let (:) _ arg = _fε'3 in arg) }) _25 (let (:) _ arg = _fε'3 in arg) undefined undefined _28 _26 (0::Int) ((0::Int) + (π'13 * (1::Int))) ((10::Int) + (π'13 * (0::Int))) (1::Int)) }

lam'4 acc' = \c -> ord c + (acc' * (31::Int))

lam' n'5 = \ds -> case n'5 == (0::Int) of { True -> []; False -> (let (:) arg _ = ds in arg) : (call'4 (call'5 False undefined (let (:) _ arg = ds in arg) undefined (let (:) _ arg = ds in arg) undefined undefined) (n'5 - (1::Int))) }

call'2 = let
  _30 = (2::Int) + (2::Int)
  _32 = sel1 (call'7 _30)
  _33 = (1::Int) : _32
  _31 = (2::Int) : _33
  in (,,,,) ((1::Int) : _31) _32 (sel2 (call'7 _30)) _33 _31

takeDigits = lam

call'9 d' = (call'3 (sel5 call'2) (sel3 call'2) (sel1 call'2) (sel2 call'2) (sel4 call'2) d')
