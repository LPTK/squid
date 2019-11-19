-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  13
-- Incl. one-shot:   0
-- Case reductions:  3
-- Field reductions: 22
-- Case commutings:  0
-- Total nodes: 1010; Boxes: 180; Branches: 149
-- Apps: 337; Lams: 10

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main (main,e,takeDigits,ratTrans,hash,eContFrac) where

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

lam'2 flag_ds' flag_ds'' flag_ds''2 _0 _1 _2 _3 _4 ds'2 π π' _5 π'2 _6 _7 = \xs -> let
  π'4 = case flag_ds'' of { True -> _7; False -> (case flag_ds''2 of { True -> _3; False -> (case flag_ds' of { True -> _5; False -> (let (,,,) _ _ _ arg = ds'2 in arg) }) }) }
  π'6 = case flag_ds'' of { True -> _2; False -> (case flag_ds''2 of { True -> _1; False -> (case flag_ds' of { True -> π'; False -> (let (,,,) _ arg _ _ = ds'2 in arg) }) }) }
  π'5 = case flag_ds'' of { True -> _0; False -> (case flag_ds''2 of { True -> π; False -> (case flag_ds' of { True -> π'2; False -> (let (,,,) arg _ _ _ = ds'2 in arg) }) }) }
  π'3 = case flag_ds'' of { True -> _6; False -> (case flag_ds''2 of { True -> π'; False -> (case flag_ds' of { True -> _4; False -> (let (,,,) _ _ arg _ = ds'2 in arg) }) }) }
  _8 = div π'6 π'4
  ψ = case ((signum π'3 == signum π'4) || (abs π'3 < abs π'4)) && ((((π'3 + π'4) * _8) <= (π'5 + π'6)) && ((((π'3 + π'4) * _8) + (π'3 + π'4)) > (π'5 + π'6))) of { True -> _8 : (call xs (π'5 - (_8 * π'3)) π'4 (π'6 - (_8 * π'4)) π'3); False -> (call' False False False (let (:) _ arg = xs in arg) undefined undefined undefined undefined (let (:) _ arg = xs in arg) undefined undefined undefined (π'5 + ((let (:) arg _ = xs in arg) * π'6)) (π'3 + ((let (:) arg _ = xs in arg) * π'4)) π'6 π'4) }
  in case flag_ds'' of { True -> ψ; False -> (case flag_ds''2 of { True -> ψ; False -> (case flag_ds' of { True -> ψ; False -> (case ds'2 of { (,,,) ρ ρ' ρ'2 ρ'3 -> ψ }) }) }) }

hash = Data.Foldable.foldl' lam'3 (0::Int)

call'2 _9 _10 _11 _12 _13 _14 _15 n' = case n' == (0::Int) of { True -> []; False -> _10 : (call'3 (call'4 True _15 _9 (let (:) _ arg = undefined in arg) _11 _12 _14 _13 (10::Int) (0::Int) (0::Int) (1::Int)) (n' - (1::Int))) }

call' flag_xs_xs_xs_ds flag_ds_xs flag_xs_ds_xs π'7 _16 _17 _18 _19 _fε _20 _21 _22 _23 _24 π'8 π'9 = let
  _fε' = case flag_xs_xs_xs_ds of { True -> _20; False -> (let (:) _ arg = _fε in arg) }
  _fε'2 = case flag_ds_xs of { True -> _21; False -> (case flag_xs_ds_xs of { True -> (1::Int); False -> (case flag_xs_xs_xs_ds of { True -> (1::Int); False -> (let (:) arg _ = _fε in arg) }) }) }
  _25 = div _23 _24
  in case ((signum π'9 == signum _24) || (abs π'9 < abs _24)) && ((((π'9 + _24) * _25) <= (π'8 + _23)) && ((((π'9 + _24) * _25) + (π'9 + _24)) > (π'8 + _23))) of { True -> _25 : (call π'7 (π'8 - (_25 * π'9)) _24 (_23 - (_25 * _24)) π'9); False -> (call' flag_xs_ds_xs False flag_ds_xs (case flag_ds_xs of { True -> _19; False -> (case flag_xs_ds_xs of { True -> _16; False -> _fε' }) }) _22 undefined _17 undefined _fε' _18 undefined undefined (π'8 + (_fε'2 * _23)) (π'9 + (_fε'2 * _24)) _23 _24) }

call'5 _26 = _26

lam'3 = \acc -> (lam'4 acc)

e = lam'5

main = Criterion.Main.defaultMain ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "main"#) $ Criterion.Measurement.Types.whnf lam'6 (420::Int)) : [])

lam'5 = \n'2 -> 
  let ret = (call'6 (2::Int)) in
  (call'2 (sel1 ret) (2::Int) (sel5 ret) (sel3 ret) (2::Int) (sel4 ret) (sel2 ret) n'2)

call'7 _27 = 
  let _28 = _27 : ((1::Int) : sel1 (call'7 (_27 + (2::Int)))) in
  (,) ((1::Int) : _28) _28

ratTrans = lam'7

lam'7 = \ds' -> (lam'2 False False False undefined undefined undefined undefined undefined ds' undefined undefined undefined undefined undefined undefined)

call xs' _29 π'10 _30 π'11 = 
  let _31 = div π'10 _30 in
  case ((signum _29 == signum _30) || (abs _29 < abs _30)) && ((((_29 + _30) * _31) <= (π'11 + π'10)) && ((((_29 + _30) * _31) + (_29 + _30)) > (π'11 + π'10))) of { True -> _31 : (call xs' (π'11 - (_31 * _29)) _30 (π'10 - (_31 * _30)) _29); False -> (call' False False False (let (:) _ arg = xs' in arg) undefined undefined undefined undefined (let (:) _ arg = xs' in arg) undefined undefined undefined (π'11 + ((let (:) arg _ = xs' in arg) * π'10)) (_29 + ((let (:) arg _ = xs' in arg) * _30)) π'10 _30) }

call'8 n'3 = n'3

lam'6 = \d -> Data.Foldable.foldl' lam'3 (0::Int) (GHC.Show.show (call'9 d))

eContFrac = (2::Int) : sel1 (call'6 (2::Int))

call'3 ψ' n'4 = case n'4 == (0::Int) of { True -> []; False -> (let (:) arg _ = ψ' in arg) : (call'3 (call'4 False undefined (let (:) _ arg = ψ' in arg) (let (:) _ arg = ψ' in arg) undefined undefined undefined undefined (10::Int) (0::Int) (0::Int) (1::Int)) (n'4 - (1::Int))) }

call'4 flag_ds _32 π'12 _fε'3 _33 _34 _35 _36 _37 _38 _39 _40 = let
  π'13 = case flag_ds of { True -> (1::Int); False -> (let (:) arg _ = _fε'3 in arg) }
  _41 = div _38 _40
  in case ((signum _39 == signum _40) || (abs _39 < abs _40)) && ((((_39 + _40) * _41) <= (_37 + _38)) && ((((_39 + _40) * _41) + (_39 + _40)) > (_37 + _38))) of { True -> _41 : (call π'12 (_37 - (_41 * _39)) _40 (_38 - (_41 * _40)) _39); False -> (call' False flag_ds False (case flag_ds of { True -> _33; False -> (let (:) _ arg = _fε'3 in arg) }) undefined _34 undefined _35 (let (:) _ arg = _fε'3 in arg) undefined _36 _32 (_37 + (π'13 * _38)) (_39 + (π'13 * _40)) _38 _40) }

lam'4 acc' = \c -> ord c + (acc' * (31::Int))

lam' n'5 = \ds -> case n'5 == (0::Int) of { True -> []; False -> (let (:) arg _ = ds in arg) : (call'3 (call'4 False undefined (let (:) _ arg = ds in arg) (let (:) _ arg = ds in arg) undefined undefined undefined undefined (10::Int) (0::Int) (0::Int) (1::Int)) (n'5 - (1::Int))) }

call'6 _42 = let
  ret' = (call'7 (_42 + (2::Int)))
  _44 = sel1 ret'
  _45 = (1::Int) : _44
  _43 = _42 : _45
  in (,,,,) ((1::Int) : _43) _44 (sel2 ret') _45 _43

takeDigits = lam

call'9 d' = 
  let ret = (call'6 (2::Int)) in
  (call'2 (sel1 ret) (2::Int) (sel5 ret) (sel3 ret) (2::Int) (sel4 ret) (sel2 ret) d')
