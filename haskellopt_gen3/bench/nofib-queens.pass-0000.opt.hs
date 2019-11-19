-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  20
-- Incl. one-shot:   0
-- Case reductions:  204
-- Field reductions: 79
-- Case commutings:  753
-- Total nodes: 2537; Boxes: 1364; Branches: 144
-- Apps: 134; Lams: 5

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main (main,nsoln,enumFromTo_mine,length_mine) where

import Data.Tuple.Select
import Criterion.Main
import Criterion.Measurement.Types
import GHC.Base
import GHC.CString
import GHC.Classes
import GHC.Num
import GHC.Types

length_mine = lam

call _0 = _0

call' _1 to = let
  _2 = _1 > to
  ψ = sel3 (call' (_1 + (1::Int)) to)
  in (,,) ψ _2 (case _2 of { True -> []; False -> _1 : ψ })

lam' = \from -> (lam'2 from)

call'2 flag_ds'3 flag_ds' _cfε ψ' _3 _4 π _5 _cfε' π' π'2 π'3 π'4 π'5 _cfε'2 _6 _7 _8 _9 nq π'6 = let
  ret = (call'3 flag_ds' _cfε' _6 π'5 nq)
  _ccε = sel2 ret
  _cfε'4 = case flag_ds'3 of { True -> _4; False -> (let (:) _ arg = _cfε in arg) }
  _cfε'3 = case flag_ds'3 of { True -> _3; False -> (let (:) arg _ = _cfε in arg) }
  _10 = (_cfε'3 : π') : sel5 (call'2 False flag_ds' _cfε'4 undefined undefined undefined π _5 _cfε' π' π'2 π'3 π'4 π'5 _cfε'2 _6 _7 _8 _9 nq π'6)
  ψ'3 = case (call'4 flag_ds' _7 _8 π _cfε'2 _5 π'6 _9 π'3 π'4 π'2 (1::Int) _cfε'3) of { True -> _10; False -> sel5 (call'5 False flag_ds' _cfε'4 undefined undefined undefined π _5 _cfε' π' π'2 π'3 π'5 π'4 _cfε'2 _6 _7 _8 nq _9 π'6) }
  t = case _6 of { True -> _10; False -> (case _9 of { True -> (case π'3 of { (:) ρ'4 ρ'5 -> ψ'3; [] -> _10 }); False -> (case _7 of { True -> ψ'3; False -> (case π'2 of { (:) ρ'6 ρ'7 -> ψ'3; [] -> _10 }) }) }) }
  ψ'2 = case flag_ds' of { True -> t; False -> (case _cfε'2 of { (:) ρ'2 ρ'3 -> ψ'3; [] -> _10 }) }
  in (,,,,,) ψ'2 t ψ' _ccε (case _cfε of { (:) ρ ρ' -> ψ'2; [] -> _ccε }) (sel6 ret)

call'6 nq' nq'2 = let
  ret' = (call'7 (nq' - (1::Int)) nq'2)
  _13 = sel2 ret'
  _14 = sel5 ret'
  t' = sel10 ret'
  _12 = sel1 ret'
  _11 = sel4 ret'
  ret'2 = (call'8 (sel11 ret') (sel9 ret') (sel6 ret') _12 (sel7 ret') _14 t' (sel3 ret') (sel16 ret') _11 (sel18 ret') (sel14 ret') (sel8 ret') (sel17 ret') (sel13 ret') _13 (sel15 ret') nq'2)
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) (sel3 ret'2) _11 (sel21 ret'2) (nq' == (0::Int)) (sel7 ret'2) _12 (sel15 ret'2) (sel22 ret'2) (sel12 ret'2) (sel10 ret'2) (sel25 ret'2) _13 _14 (sel29 ret'2) (sel4 ret'2) (sel20 ret'2) t' (sel17 ret'2) (sel2 ret'2) (sel23 ret'2) (sel8 ret'2) (sel5 ret'2) (sel26 ret'2) (sel27 ret'2) (sel18 ret'2) (sel19 ret'2) (sel6 ret'2) (sel9 ret'2) (sel16 ret'2) (sel11 ret'2) (sel12 ret')

main = Criterion.Main.defaultMain ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "main"#) $ Criterion.Measurement.Types.whnf lam'3 (8::Int)) : [])

call'9 flag_ds'' _15 _16 _17 _18 _19 ψ'4 π'7 _20 _cfε'5 π'8 π'9 π'10 π'11 π'12 _cfε'6 _21 _22 _23 nq'3 _24 π'13 = let
  ret'3 = (call'2 True flag_ds'' (let (:) _ arg = undefined in arg) ψ'4 _17 _16 π'7 _20 _cfε'5 π'8 π'9 π'10 π'12 π'11 _cfε'6 _21 _22 _23 _24 nq'3 π'13)
  _27 = case sel3 ret'3 of { (:) ρ'14 ρ'15 -> sel1 ret'3; [] -> sel4 ret'3 }
  ret = (call'3 flag_ds'' _cfε'5 _21 π'11 nq'3)
  _25 = _15 : π'8
  ret'5 = (call'5 True flag_ds'' (let (:) _ arg = undefined in arg) _17 _18 _16 π'7 _20 _cfε'5 π'8 π'9 π'10 π'11 π'12 _cfε'6 _21 _22 _23 nq'3 _24 π'13)
  ret'4 = (call'4 flag_ds'' _22 _23 π'7 _cfε'6 _20 π'13 _24 π'10 π'12 π'9 (1::Int) _15)
  _26 = _25 : _27
  ψ'5 = case ret'4 of { True -> _26; False -> sel3 ret'5 }
  ψ'6 = case π'9 of { (:) ρ'12 ρ'13 -> ψ'5; [] -> _26 }
  in (,,,,,,,,,,,,,,,,,,,) (sel1 ret) (sel2 ret'3) ret'4 (sel7 ret) (sel5 ret) (sel4 ret) _25 (sel6 ret'5) (case _19 of { True -> sel2 ret; False -> (case flag_ds'' of { True -> (case _21 of { True -> _26; False -> (case _24 of { True -> (case π'10 of { (:) ρ'8 ρ'9 -> ψ'5; [] -> _26 }); False -> (case _22 of { True -> ψ'5; False -> ψ'6 }) }) }); False -> (case _cfε'6 of { (:) ρ'10 ρ'11 -> ψ'5; [] -> _26 }) }) }) (sel6 ret) (sel1 ret'5) (sel4 ret'5) _27 ψ'4 (sel6 ret'3) (sel8 ret) (sel7 ret'5) (sel3 ret) (sel2 ret'5) ψ'6

call'10 flag_ds _28 t'2 π'14 _29 π'15 π'16 π'17 π'18 π'19 _30 _cfε'7 _31 π'20 t'3 _32 π'21 π'22 π'23 ψ'7 π'24 _33 _34 π'25 _35 _36 π'26 = 
  let _37 = (call'10 False undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined (case flag_ds of { True -> (case _30 of { True -> (case _33 of { True -> (let (:) _ arg = π'17 in arg); False -> (case ψ'7 of { (:) ρ'16 ρ'17 -> (let (:) _ arg = t'2 in arg); [] -> (let (:) _ arg = t'3 in arg) }) }); False -> (case _33 of { True -> (let (:) _ arg = π'20 in arg); False -> (case _29 of { True -> (case π'19 of { (:) ρ'18 ρ'19 -> (case _35 of { True -> (case ψ'7 of { (:) ρ'20 ρ'21 -> (let (:) _ arg = t'2 in arg); [] -> (let (:) _ arg = t'3 in arg) }); False -> (let (:) _ arg = π'25 in arg) }); [] -> (case ψ'7 of { (:) ρ'22 ρ'23 -> (let (:) _ arg = t'2 in arg); [] -> (let (:) _ arg = t'3 in arg) }) }); False -> (case _31 of { (:) ρ'24 ρ'25 -> (case _28 of { True -> (case π'15 of { (:) ρ'26 ρ'27 -> (case _35 of { True -> (case ψ'7 of { (:) ρ'28 ρ'29 -> (let (:) _ arg = t'2 in arg); [] -> (let (:) _ arg = t'3 in arg) }); False -> (let (:) _ arg = π'24 in arg) }); [] -> (case ψ'7 of { (:) ρ'30 ρ'31 -> (let (:) _ arg = t'2 in arg); [] -> (let (:) _ arg = t'3 in arg) }) }); False -> (let (:) _ arg = π'14 in arg) }); [] -> (case π'22 of { (:) ρ'32 ρ'33 -> (case _35 of { True -> (case ψ'7 of { (:) ρ'34 ρ'35 -> (let (:) _ arg = t'2 in arg); [] -> (let (:) _ arg = t'3 in arg) }); False -> (let (:) _ arg = π'21 in arg) }); [] -> (case ψ'7 of { (:) ρ'36 ρ'37 -> (let (:) _ arg = t'2 in arg); [] -> (let (:) _ arg = t'3 in arg) }) }) }) }) }) }); False -> (let (:) _ arg = _cfε'7 in arg) }) undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined (let (:) _ arg = undefined in arg) undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg)) + (1::Int) in
  case flag_ds of { True -> (case _32 of { True -> (0::Int); False -> (case _30 of { True -> (case _33 of { True -> (case π'17 of { (:) ρ'38 ρ'39 -> _37; [] -> (0::Int) }); False -> (case _36 of { (:) ρ'40 ρ'41 -> _37; [] -> (0::Int) }) }); False -> (case _33 of { True -> (case π'20 of { (:) ρ'42 ρ'43 -> _37; [] -> (0::Int) }); False -> (case _29 of { True -> (case π'19 of { (:) ρ'44 ρ'45 -> (case _35 of { True -> (case _36 of { (:) ρ'46 ρ'47 -> _37; [] -> (0::Int) }); False -> (case π'25 of { (:) ρ'48 ρ'49 -> _37; [] -> (0::Int) }) }); [] -> (case _36 of { (:) ρ'50 ρ'51 -> _37; [] -> (0::Int) }) }); False -> (case _31 of { (:) ρ'52 ρ'53 -> (case _28 of { True -> (case _34 of { (:) ρ'54 ρ'55 -> (case _35 of { True -> (case _36 of { (:) ρ'56 ρ'57 -> _37; [] -> (0::Int) }); False -> (case π'24 of { (:) ρ'58 ρ'59 -> _37; [] -> (0::Int) }) }); [] -> (case _36 of { (:) ρ'60 ρ'61 -> _37; [] -> (0::Int) }) }); False -> (case π'14 of { (:) ρ'62 ρ'63 -> _37; [] -> (0::Int) }) }); [] -> (case π'22 of { (:) ρ'64 ρ'65 -> (case _35 of { True -> (case _36 of { (:) ρ'66 ρ'67 -> _37; [] -> (0::Int) }); False -> (case π'21 of { (:) ρ'68 ρ'69 -> _37; [] -> (0::Int) }) }); [] -> (case _36 of { (:) ρ'70 ρ'71 -> _37; [] -> (0::Int) }) }) }) }) }) }) }); False -> (case _cfε'7 of { (:) ρ'72 ρ'73 -> _37; [] -> (0::Int) }) }

call'7 _38 nq'4 = let
  ret' = (call'7 (_38 - (1::Int)) nq'4)
  _41 = sel2 ret'
  _39 = sel4 ret'
  _40 = sel1 ret'
  ret'2 = (call'8 (sel11 ret') (sel9 ret') (sel6 ret') _40 (sel7 ret') (sel5 ret') (sel10 ret') (sel3 ret') (sel16 ret') _39 (sel18 ret') (sel14 ret') (sel8 ret') (sel17 ret') (sel13 ret') _41 (sel15 ret') nq'4)
  in (,,,,,,,,,,,,,,,,,) (sel17 ret'2) _39 (sel21 ret'2) (_38 == (0::Int)) (sel28 ret'2) _40 (sel24 ret'2) (sel22 ret'2) (sel26 ret'2) (sel30 ret'2) (sel13 ret'2) (sel14 ret'2) _41 (sel9 ret'2) (sel11 ret'2) (sel20 ret'2) (sel1 ret'2) (sel12 ret')

call'3 flag_ds''2 _cfε'8 _42 π'27 nq'5 = let
  ret'6 = (call'11 nq'5 (1::Int))
  _cfε'9 = case flag_ds''2 of { True -> (let (:) arg _ = π'27 in arg); False -> (let (:) arg _ = _cfε'8 in arg) }
  _43 = sel1 ret'6
  ret'7 = (call'9 False (1::Int) (sel6 ret'6) (sel3 ret'6) (sel2 ret'6) _43 (sel5 ret'6) undefined undefined (case flag_ds''2 of { True -> (let (:) _ arg = π'27 in arg); False -> (let (:) _ arg = _cfε'8 in arg) }) _cfε'9 undefined (let (:) arg _ = undefined in arg) (let (:) _ arg = undefined in arg) undefined _cfε'9 undefined undefined undefined nq'5 undefined undefined)
  t'4 = sel9 ret'7
  π'28 = case _42 of { True -> []; False -> (case π'27 of { (:) ρ'76 ρ'77 -> t'4; [] -> [] }) }
  _ccε' = case _cfε'8 of { (:) ρ'74 ρ'75 -> t'4; [] -> [] }
  in (,,,,,,,) (sel4 ret'7) (case flag_ds''2 of { True -> π'28; False -> _ccε' }) (let (:) arg _ = π'27 in arg) (sel3 ret'7) _43 π'28 _ccε' (sel19 ret'7)

enumFromTo_mine = lam'

call'12 from' to' = let
  _44 = from' > to'
  _46 = from' + (1::Int)
  ret'8 = (call' _46 to')
  ψ'8 = sel3 ret'8
  _45 = from' : ψ'8
  in (,,,,,,) ψ'8 _45 (sel1 ret'8) _44 (case _44 of { True -> []; False -> _45 }) (sel2 ret'8) _46

call'13 _47 = _47

lam'4 = \nq'6 -> 
  let ret'9 = (call'6 nq'6 nq'6) in
  (call'14 (sel15 ret'9) (sel6 ret'9) (sel2 ret'9) (sel14 ret'9) (sel25 ret'9) (sel27 ret'9) (sel26 ret'9) (sel4 ret'9) (sel7 ret'9) (sel5 ret'9) (sel13 ret'9) (sel23 ret'9) (sel8 ret'9) (sel19 ret'9) (sel16 ret'9) (sel18 ret'9) (sel3 ret'9) (sel30 ret'9) (sel31 ret'9) (sel9 ret'9) (sel24 ret'9) (sel1 ret'9) (sel21 ret'9) (sel12 ret'9) (sel28 ret'9) (sel22 ret'9) (sel10 ret'9) (sel11 ret'9) (sel20 ret'9) (sel17 ret'9) (sel29 ret'9))

call'15 x = x

call'14 _48 _49 _50 π'29 _ccε'2 t'5 _cfε'10 _51 _52 π'30 _53 π'31 t'6 _54 π'32 _55 π'33 t'7 _56 π'34 t'8 t'9 ψ'9 _57 _58 π'35 t'10 π'36 _ccε'3 t'11 _59 = let
  _60 = (call'10 True _57 t'9 (let (:) _ arg = π'30 in arg) _49 π'33 (let (:) _ arg = t'7 in arg) (let (:) _ arg = t'6 in arg) (let (:) _ arg = t'7 in arg) π'32 _50 (let (:) _ arg = undefined in arg) _56 (let (:) _ arg = t'6 in arg) t'10 _51 (let (:) _ arg = t'7 in arg) π'31 (let (:) _ arg = t'7 in arg) ψ'9 (let (:) _ arg = t'7 in arg) _55 _53 (let (:) _ arg = t'7 in arg) _58 _54 (let (:) _ arg = π'30 in arg)) + (1::Int)
  ψ'11 = case _58 of { True -> _60; False -> (case π'35 of { (:) ρ'94 ρ'95 -> (case _50 of { True -> _60; False -> (case _49 of { True -> (case π'32 of { (:) ρ'96 ρ'97 -> (case _48 of { True -> _60; False -> (case _ccε'2 of { (:) ρ'98 ρ'99 -> _60; [] -> (0::Int) }) }); [] -> _60 }); False -> (case _56 of { (:) ρ'100 ρ'101 -> (case _57 of { True -> (case π'33 of { (:) ρ'102 ρ'103 -> (case _48 of { True -> _60; False -> (case _ccε'2 of { (:) ρ'104 ρ'105 -> _60; [] -> (0::Int) }) }); [] -> _60 }); False -> (case π'34 of { (:) ρ'106 ρ'107 -> _60; [] -> (0::Int) }) }); [] -> (case π'31 of { (:) ρ'108 ρ'109 -> (case _48 of { True -> _60; False -> (case _ccε'2 of { (:) ρ'110 ρ'111 -> _60; [] -> (0::Int) }) }); [] -> _60 }) }) }) }); [] -> (case t'5 of { (:) ρ'112 ρ'113 -> _60; [] -> (0::Int) }) }) }
  ψ'10 = case _55 of { True -> (case _50 of { True -> (0::Int); False -> (case π'29 of { (:) ρ'80 ρ'81 -> (case _52 of { True -> (case _ccε'3 of { (:) ρ'82 ρ'83 -> _60; [] -> (0::Int) }); False -> (case _cfε'10 of { (:) ρ'84 ρ'85 -> (case _59 of { True -> _60; False -> (case t'8 of { (:) ρ'86 ρ'87 -> _60; [] -> (0::Int) }) }); [] -> _60 }) }); [] -> (0::Int) }) }); False -> (case _50 of { True -> _60; False -> (case _49 of { True -> (case π'32 of { (:) ρ'88 ρ'89 -> ψ'11; [] -> _60 }); False -> (case _57 of { True -> (case _53 of { (:) ρ'90 ρ'91 -> ψ'11; [] -> _60 }); False -> (case π'36 of { (:) ρ'92 ρ'93 -> ψ'11; [] -> _60 }) }) }) }) }
  in case _51 of { True -> _60; False -> (case _50 of { True -> ψ'10; False -> (case t'11 of { (:) ρ'78 ρ'79 -> ψ'10; [] -> (0::Int) }) }) }

lam = \ds -> case ds of { (:) ρ'114 ρ'115 -> (call'10 False undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined ρ'115 undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined (let (:) _ arg = undefined in arg) undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg)) + (1::Int); [] -> (0::Int) }

call'16 flag_ds'_ds'2 π'37 π'38 π'39 π'40 π'41 _61 _cfε'11 _62 _63 d x' = let
  _cfε'12 = case flag_ds'_ds'2 of { True -> (case _61 of { True -> (let (:) arg _ = π'40 in arg); False -> (case _62 of { True -> (let (:) arg _ = π'39 in arg); False -> (let (:) arg _ = π'37 in arg) }) }); False -> (let (:) arg _ = _cfε'11 in arg) }
  _64 = (x' /= _cfε'12) && ((x' /= (_cfε'12 + d)) && ((x' /= (_cfε'12 - d)) && (call'16 False (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (case flag_ds'_ds'2 of { True -> (case _61 of { True -> (let (:) _ arg = π'40 in arg); False -> (case _62 of { True -> (case _63 of { True -> (let (:) _ arg = π'41 in arg); False -> (let (:) _ arg = π'38 in arg) }); False -> (let (:) _ arg = π'37 in arg) }) }); False -> (let (:) _ arg = _cfε'11 in arg) }) undefined undefined (d + (1::Int)) x')))
  in case flag_ds'_ds'2 of { True -> (case _61 of { True -> (case π'40 of { (:) ρ'116 ρ'117 -> _64; [] -> True }); False -> (case _62 of { True -> True; False -> (case π'37 of { (:) ρ'118 ρ'119 -> _64; [] -> True }) }) }); False -> (case _cfε'11 of { (:) ρ'120 ρ'121 -> _64; [] -> True }) }

lam'2 from'2 = \to'2 -> sel5 (call'12 from'2 to'2)

call'4 flag_ds''3 _65 _66 π'42 _cfε'13 _67 π'43 _68 π'44 π'45 π'46 d' x'2 = 
  let π'47 = case flag_ds''3 of { True -> (case _68 of { True -> (let (:) arg _ = π'44 in arg); False -> (case _65 of { True -> (let (:) arg _ = _66 in arg); False -> (let (:) arg _ = π'46 in arg) }) }); False -> (let (:) arg _ = _cfε'13 in arg) } in
  (x'2 /= π'47) && ((x'2 /= (π'47 + d')) && ((x'2 /= (π'47 - d')) && (call'16 flag_ds''3 (let (:) _ arg = π'46 in arg) π'45 π'42 (let (:) _ arg = π'44 in arg) π'43 _68 (let (:) _ arg = _cfε'13 in arg) _65 _67 (d' + (1::Int)) x'2)))

call'17 π'48 = π'48

call'8 π'49 π'50 _69 _70 π'51 _71 t'12 π'52 π'53 _72 _73 _74 t'13 π'54 _75 _76 t'14 nq'7 = let
  π'55 = case _74 of { True -> _71; False -> (let (:) arg _ = t'14 in arg) }
  π'58 = case _69 of { True -> (case π'53 of { (:) ρ'124 ρ'125 -> π'55; [] -> _71 }); False -> (case _73 of { (:) ρ'126 ρ'127 -> (case _75 of { True -> (case π'52 of { (:) ρ'128 ρ'129 -> π'55; [] -> _71 }); False -> (case π'51 of { (:) ρ'130 ρ'131 -> π'55; [] -> _71 }) }); [] -> (case π'50 of { (:) ρ'132 ρ'133 -> π'55; [] -> _71 }) }) }
  π'57 = case _76 of { True -> _71; False -> π'58 }
  π'56 = case _72 of { True -> []; False -> (case _70 of { True -> (let (:) arg _ = t'13 in arg); False -> π'57 }) }
  ret'6 = (call'11 nq'7 (1::Int))
  _77 = sel1 ret'6
  ψ'12 = sel5 ret'6
  ret'7 = (call'9 True (1::Int) (sel6 ret'6) (sel3 ret'6) (sel2 ret'6) _77 ψ'12 π'54 _69 (let (:) _ arg = undefined in arg) π'56 π'58 (let (:) arg _ = t'13 in arg) (let (:) _ arg = t'12 in arg) π'49 (let (:) arg _ = undefined in arg) _72 _76 _71 nq'7 _70 π'53)
  t'15 = sel9 ret'7
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) π'56 (sel13 ret'7) (sel2 ret'7) (sel11 ret'7) (sel14 ret'7) (sel17 ret'7) (sel20 ret'7) ψ'12 (sel3 ret'7) (sel15 ret'7) (sel19 ret'7) (sel8 ret'7) π'57 (sel4 ret'6) (sel5 ret'7) (sel6 ret'7) _77 (sel12 ret'7) (sel18 ret'7) (let (:) arg _ = t'13 in arg) _71 (sel10 ret'7) (sel1 ret'7) π'58 π'58 (let (:) arg _ = t'13 in arg) (sel16 ret'7) (sel7 ret'7) (let (:) _ arg = t'12 in arg) (case _72 of { True -> t'15; False -> (case t'12 of { (:) ρ'122 ρ'123 -> t'15; [] -> [] }) })

nsoln = lam'4

call'5 flag_ds'3' flag_ds''4 _cfε'14 _78 _79 _80 π'59 _81 _cfε'15 π'60 π'61 π'62 π'63 π'64 _cfε'16 _82 _83 _84 nq'8 _85 π'65 = let
  _cfε'17 = case flag_ds'3' of { True -> _80; False -> (let (:) _ arg = _cfε'14 in arg) }
  _cfε'18 = case flag_ds'3' of { True -> _78; False -> (let (:) arg _ = _cfε'14 in arg) }
  _86 = (_cfε'18 : π'60) : sel5 (call'2 False flag_ds''4 _cfε'17 undefined undefined undefined π'59 _81 _cfε'15 π'60 π'61 π'62 π'64 π'63 _cfε'16 _82 _83 _84 _85 nq'8 π'65)
  _ccε'4 = sel5 (call'5 False flag_ds''4 _cfε'17 undefined undefined undefined π'59 _81 _cfε'15 π'60 π'61 π'62 π'63 π'64 _cfε'16 _82 _83 _84 nq'8 _85 π'65)
  ret'4 = (call'4 flag_ds''4 _83 _84 π'59 _cfε'16 _81 π'65 _85 π'62 π'64 π'61 (1::Int) _cfε'18)
  ψ'16 = case ret'4 of { True -> _86; False -> _ccε'4 }
  ψ'15 = case π'61 of { (:) ρ'140 ρ'141 -> ψ'16; [] -> _86 }
  ψ'14 = case flag_ds''4 of { True -> (case _82 of { True -> _86; False -> (case _85 of { True -> (case π'62 of { (:) ρ'136 ρ'137 -> ψ'16; [] -> _86 }); False -> (case _83 of { True -> ψ'16; False -> ψ'15 }) }) }); False -> (case _cfε'16 of { (:) ρ'138 ρ'139 -> ψ'16; [] -> _86 }) }
  ret = (call'3 flag_ds''4 _cfε'15 _82 π'63 nq'8)
  _ccε'5 = sel2 ret
  ψ'13 = case _79 of { True -> _ccε'5; False -> ψ'14 }
  in (,,,,,,) ret'4 ψ'13 ψ'13 _ccε'4 (case _cfε'14 of { (:) ρ'134 ρ'135 -> ψ'14; [] -> _ccε'5 }) ψ'15 (sel6 ret)

call'11 nq'9 from'3 = 
  let ret'10 = (call'12 from'3 nq'9) in
  (,,,,,) (sel4 ret'10) (sel6 ret'10) (sel7 ret'10) (sel2 ret'10) (sel1 ret'10) (sel3 ret'10)

lam'3 = \n -> (call'18 n)

call'19 _87 = _87

call'18 n' = 
  let ret'9 = (call'6 n' n') in
  (call'14 (sel15 ret'9) (sel6 ret'9) (sel2 ret'9) (sel14 ret'9) (sel25 ret'9) (sel27 ret'9) (sel26 ret'9) (sel4 ret'9) (sel7 ret'9) (sel5 ret'9) (sel13 ret'9) (sel23 ret'9) (sel8 ret'9) (sel19 ret'9) (sel16 ret'9) (sel18 ret'9) (sel3 ret'9) (sel30 ret'9) (sel31 ret'9) (sel9 ret'9) (sel24 ret'9) (sel1 ret'9) (sel21 ret'9) (sel12 ret'9) (sel28 ret'9) (sel22 ret'9) (sel10 ret'9) (sel11 ret'9) (sel20 ret'9) (sel17 ret'9) (sel29 ret'9))
