-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  26
-- Incl. one-shot:   0
-- Case reductions:  16
-- Field reductions: 22
-- Case commutings:  11
-- Total nodes: 1377; Boxes: 421; Branches: 243
-- Apps: 227; Lams: 32

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main (main,avg_manual_tr,avg_manual,avg_F,sum_F,length_F,k,fold_F,foldl',fmap_F,ap_F) where

import Data.Tuple.Select
import Criterion.Main
import Criterion.Measurement.Types
import GHC.Base
import GHC.CString
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Tuple
import GHC.Types

call π s l = case π of { (:) ρ ρ' -> (call ρ' (s + ρ) (l + (1::Int))); [] -> div s l }

lam f' = \ds'4 -> (,,) (case ds'4 of { (,,) ρ'2 ρ'3 ρ'4 -> ρ'2 }) (case ds'4 of { (,,) ρ'5 ρ'6 ρ'7 -> ρ'6 }) (case ds'4 of { (,,) ρ'8 ρ'9 ρ'10 -> (lam' ρ'10 f') })

k = GHC.Enum.enumFromTo (0::Int) ((1000::Int) * (1000::Int))

lam'2 = \xs -> 
  let ret = (call' xs) in
  div (sel1 ret) (sel2 ret)

lam'3 flag_ds'3 π' ds'5 f'2 _0 = \ds'2 -> 
  let _cε = (,,) (case ds'2 of { (,,) ρ'14 ρ'15 ρ'16 -> (lam'4 False ρ'14 (case flag_ds'3 of { True -> (+); False -> (let (,,) arg _ _ = ds'5 in arg) })) }) ((,) (case ds'2 of { (,,) ρ'17 ρ'18 ρ'19 -> (case flag_ds'3 of { True -> _0; False -> (let (,,) _ arg _ = ds'5 in arg) }) }) (case ds'2 of { (,,) ρ'20 ρ'21 ρ'22 -> ρ'21 })) (case ds'2 of { (,,) ρ'23 ρ'24 ρ'25 -> (lam'5 flag_ds'3 f'2 (let (,,) _ _ arg = ds'5 in arg) ρ'25 π') }) in
  case flag_ds'3 of { True -> _cε; False -> (case ds'5 of { (,,) ρ'11 ρ'12 ρ'13 -> _cε }) }

call'2 π'2 _fε = (,,,) _fε π'2 _fε _fε

fmap_F = lam'6

call'3 π'3 π'4 f'3 = 
  let _1 = π'4 π'3 in
  (,) _1 (f'3 _1)

lam'7 = \ds -> (lam'8 False undefined undefined undefined undefined undefined undefined undefined undefined ds)

main = 
  let _2 = GHC.Enum.enumFromTo (0::Int) ((1000::Int) * (1000::Int)) in
  Criterion.Main.defaultMain (Criterion.Measurement.Types.bgroup (GHC.CString.unpackCString# "folding"#) ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "normal"#) $ Criterion.Measurement.Types.whnf lam'9 _2) : ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "manual"#) $ Criterion.Measurement.Types.whnf lam'10 _2) : ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "manual_tr"#) $ Criterion.Measurement.Types.whnf lam'10 _2) : []))) : [])

call'4 _3 = _3

avg_F = lam'9

lam'4 flag_ds'2 _fε' π'5 = \ds' -> (lam'11 flag_ds'2 False ds' _fε' ds' π'5 undefined ds' undefined)

lam'12 flag_ds_f f'4 π'6 f'5 = \z -> (lam'13 False flag_ds_f z f'4 undefined z f'5 z π'6 z undefined)

call'5 xs' s' l' = case xs' of { (:) ρ'26 ρ'27 -> (call ρ'27 (s' + ρ'26) (l' + (1::Int))); [] -> div s' l' }

call'6 = id

call'7 flag_z_ds π'7 ds'6 ds'7 π'8 _4 ds'8 _5 = let
  ret' = (call'8 (case flag_z_ds of { True -> _4; False -> (let (,) _ arg = ds'7 in arg) }))
  _6 = π'8 (case flag_z_ds of { True -> _5; False -> (let (,) arg _ = ds'6 in arg) }) π'7
  in case flag_z_ds of { True -> (,) _6 ret'; False -> (,) (case ds'8 of { (,) ρ'28 ρ'29 -> _6 }) (case ds'8 of { (,) ρ'30 ρ'31 -> ret' }) }

lam'14 = \f -> (lam'12 False f undefined f)

lam'10 = \xs'2 -> (call'5 xs'2 (0::Int) (0::Int))

lam'6 = \f'6 -> (lam f'6)

call'9 z' z'2 z'3 = (,,) z' z'2 z'3

call'10 = (,)

length_F = (,,) lam'15 (0::Int) id

call'11 _7 = _7

call'8 n = n + (1::Int)

call'12 π'9 _fε'2 = (,) π'9 _fε'2

lam'16 = \ds'3 -> (lam'3 False undefined ds'3 undefined undefined)

lam'17 n' = \ds'9 -> n' + (1::Int)

call'13 π'10 = 
  let ret'2 = (call'13 (let (:) _ arg = π'10 in arg)) in
  (,) (case π'10 of { (:) ρ'32 ρ'33 -> sel1 ret'2 + ρ'32; [] -> (0::Int) }) (case π'10 of { (:) ρ'34 ρ'35 -> sel2 ret'2 + (1::Int); [] -> (0::Int) })

foldl' = lam'14

call'14 f'7 = (,) f'7 f'7

lam'9 = \xs'3 -> let
  ret'4 = (call'16 (0::Int) (0::Int))
  ret'3 = (call'15 xs'3 (sel2 ret'4) id (sel1 ret'4) (sel4 ret'4) (,) (0::Int) (0::Int) (sel3 ret'4) undefined)
  _9 = sel1 ret'3
  ψ = sel2 ret'3
  ψ'2 = case xs'3 of { (:) ρ'48 ρ'49 -> (case ψ of { (,) ρ'50 ρ'51 -> _9 }); [] -> _9 }
  _8 = sel3 ret'3
  ψ' = case xs'3 of { (:) ρ'44 ρ'45 -> (case ψ of { (,) ρ'46 ρ'47 -> _8 }); [] -> _8 }
  in div (case xs'3 of { (:) ρ'36 ρ'37 -> (case ψ of { (,) ρ'38 ρ'39 -> ψ' }); [] -> ψ' }) (case xs'3 of { (:) ρ'40 ρ'41 -> (case ψ of { (,) ρ'42 ρ'43 -> ψ'2 }); [] -> ψ'2 })

call'15 xs'4 π'11 π'12 π'13 λ f'8 _10 _11 _12 ds'10 = let
  ret'5 = (call'2 _12 (let (,,) _ arg _ = ds'10 in arg))
  ret'7 = (,) λ (let (,,) arg _ _ = ds'10 in arg)
  ψ'3 = sel1 (call'18 True xs'4 (sel2 ret'5) (sel2 ret'7) _11 (sel1 ret'5) (sel1 ret'7) π'11 (sel3 ret'5) (sel4 ret'5) _10)
  ret'6 = (call'17 True _10 _11 (let (,,) _ arg _ = ds'10 in arg) ψ'3 xs'4 f'8 π'13 π'12)
  in (,,) (sel3 ret'6) ψ'3 (sel1 ret'6)

avg_manual = lam'2

call'19 _13 = _13

lam'5 flag_ds'3' f'9 _fε'3 π'14 π'15 = \ds'11 -> case ds'11 of { (,) ρ'52 ρ'53 -> (case flag_ds'3' of { True -> sel2 (call'3 ρ'52 π'15 f'9); False -> _fε'3 ρ'52 }) (π'14 ρ'53) }

lam'8 flag_ds π'16 π'17 π'18 λ' f'10 _14 _15 _16 ds'12 = \xs'5 -> let
  ret'5 = (call'2 (case flag_ds of { True -> _16; False -> (let (,,) _ arg _ = ds'12 in arg) }) (let (,,) _ arg _ = ds'12 in arg))
  ret'7 = (,) (case flag_ds of { True -> λ'; False -> (let (,,) arg _ _ = ds'12 in arg) }) (let (,,) arg _ _ = ds'12 in arg)
  ret'8 = (call'18 flag_ds xs'5 (sel2 ret'5) (sel2 ret'7) _15 (sel1 ret'5) (sel1 ret'7) π'16 (sel3 ret'5) (sel4 ret'5) _14)
  _17 = case flag_ds of { True -> sel2 (call'17 flag_ds _14 _15 (let (,,) _ arg _ = ds'12 in arg) (sel1 ret'8) xs'5 f'10 π'18 π'17); False -> (let (,,) _ _ arg = ds'12 in arg) (sel2 ret'8) }
  in case flag_ds of { True -> _17; False -> (case ds'12 of { (,,) ρ'54 ρ'55 ρ'56 -> _17 }) }

avg_manual_tr = lam'10

ap_F = lam'16

call'18 flag_ds' xs'6 z'4 f'11 _18 z'5 f'12 π'19 z'6 z'7 _19 = let
  ret'9 = (call'14 f'12)
  ret'11 = (,,) z'5 z'6 z'7
  ret'10 = (call'20 (let (:) _ arg = xs'6 in arg) (case flag_ds' of { True -> (call'7 flag_ds' (let (:) arg _ = xs'6 in arg) (sel1 ret'11) (sel3 ret'11) π'19 _19 (sel2 ret'11) _18); False -> f'11 z'4 (let (:) arg _ = xs'6 in arg) }) (sel2 ret'9) (sel1 ret'9))
  in (,) ret'10 (case xs'6 of { (:) ρ'57 ρ'58 -> ret'10; [] -> z'4 })

call'21 _20 = _20

call'22 _21 = _21

lam'13 flag_ds_z flag_f_ds z'8 f'13 _22 z'9 f'14 z'10 π'20 z'11 _23 = \ds'13 -> let
  ret'9 = (call'14 f'14)
  ret'11 = (,,) z'9 z'10 z'11
  in case ds'13 of { (:) ρ'59 ρ'60 -> (call'20 ρ'60 (case flag_f_ds of { True -> (call'7 flag_ds_z ρ'59 (sel1 ret'11) (sel3 ret'11) π'20 _23 (sel2 ret'11) _22); False -> f'13 z'8 ρ'59 }) (sel2 ret'9) (sel1 ret'9)); [] -> z'8 }

call'16 _24 _25 = (,,,) id (+) ((,) _25 _24) (lam'4 True (let (,,) arg _ _ = undefined in arg) (+))

call'20 π'21 z'12 f'15 f'16 = 
  let ret'9 = (call'14 f'16) in
  case π'21 of { (:) ρ'61 ρ'62 -> (call'20 ρ'62 (f'15 z'12 ρ'61) (sel2 ret'9) (sel1 ret'9)); [] -> z'12 }

sum_F = (,,) (+) (0::Int) id

lam'11 flag_ds'2' flag_z_ds'_ds ds'14 _fε'4 ds'15 π'22 _26 ds'16 _27 = \a -> let
  _28 = π'22 (case flag_z_ds'_ds of { True -> _27; False -> (let (,) arg _ = ds'14 in arg) }) a
  π'23 = case flag_z_ds'_ds of { True -> _26; False -> (let (,) _ arg = ds'15 in arg) }
  _29 = case flag_ds'2' of { True -> (call'8 π'23); False -> _fε'4 π'23 a }
  in case flag_z_ds'_ds of { True -> (,) _28 _29; False -> (,) (case ds'16 of { (,) ρ'63 ρ'64 -> _28 }) (case ds'16 of { (,) ρ'65 ρ'66 -> _29 }) }

call'23 π'24 = π'24

lam'15 = \n'2 -> (lam'17 n'2)

lam' π'25 f'17 = \x -> f'17 (π'25 x)

fold_F = lam'7

call'17 flag_ds'2 _30 _31 _fε'5 ψ'4 xs'7 f'18 π'26 π'27 = let
  ret'12 = (call'3 (case xs'7 of { (:) ρ'67 ρ'68 -> (let (,) arg _ = ψ'4 in arg); [] -> (case flag_ds'2 of { True -> _31; False -> (let (,) arg _ = _fε'5 in arg) }) }) π'27 f'18)
  _33 = π'26 (case xs'7 of { (:) ρ'75 ρ'76 -> (let (,) _ arg = ψ'4 in arg); [] -> (case flag_ds'2 of { True -> _30; False -> (let (,) _ arg = _fε'5 in arg) }) })
  _32 = sel2 ret'12 _33
  in (,,) (sel1 ret'12) (case xs'7 of { (:) ρ'69 ρ'70 -> (case ψ'4 of { (,) ρ'71 ρ'72 -> _32 }); [] -> (case flag_ds'2 of { True -> _32; False -> (case _fε'5 of { (,) ρ'73 ρ'74 -> _32 }) }) }) _33

call' xs'8 = 
  let ret'2 = (call'13 (let (:) _ arg = xs'8 in arg)) in
  (,) (case xs'8 of { (:) ρ'77 ρ'78 -> sel1 ret'2 + ρ'77; [] -> (0::Int) }) (case xs'8 of { (:) ρ'79 ρ'80 -> sel2 ret'2 + (1::Int); [] -> (0::Int) })

call'24 = undefined
