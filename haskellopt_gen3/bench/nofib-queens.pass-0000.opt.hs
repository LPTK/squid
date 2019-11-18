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
  ψ = sel3 (call' (_1 + (1::Int)) to)
  _2 = _1 > to
  in (,,) ψ _2 (case _2 of { True -> []; False -> _1 : ψ })

lam' = \from -> (lam'2 from)

call'2 flag_ds'3 flag_ds' _cfε ψ' _3 _4 π _5 _cfε' π' π'2 π'3 π'4 π'5 _cfε'2 _6 _7 _8 _9 nq π'6 = let
  _ccε = sel2 (call'3 flag_ds' _cfε' _6 π'5 nq)
  _cfε'4 = case flag_ds'3 of { True -> _4; False -> (let (:) _ arg = _cfε in arg) }
  _cfε'3 = case flag_ds'3 of { True -> _3; False -> (let (:) arg _ = _cfε in arg) }
  _10 = (_cfε'3 : π') : sel5 (call'2 False flag_ds' _cfε'4 undefined undefined undefined π _5 _cfε' π' π'2 π'3 π'4 π'5 _cfε'2 _6 _7 _8 _9 nq π'6)
  ψ'3 = case (call'4 flag_ds' _7 _8 π _cfε'2 _5 π'6 _9 π'3 π'4 π'2 (1::Int) _cfε'3) of { True -> _10; False -> sel5 (call'5 False flag_ds' _cfε'4 undefined undefined undefined π _5 _cfε' π' π'2 π'3 π'5 π'4 _cfε'2 _6 _7 _8 nq _9 π'6) }
  t = case _6 of { True -> _10; False -> (case _9 of { True -> (case π'3 of { (:) ρ'4 ρ'5 -> ψ'3; [] -> _10 }); False -> (case _7 of { True -> ψ'3; False -> (case π'2 of { (:) ρ'6 ρ'7 -> ψ'3; [] -> _10 }) }) }) }
  ψ'2 = case flag_ds' of { True -> t; False -> (case _cfε'2 of { (:) ρ'2 ρ'3 -> ψ'3; [] -> _10 }) }
  in (,,,,,) ψ'2 t ψ' _ccε (case _cfε of { (:) ρ ρ' -> ψ'2; [] -> _ccε }) (sel6 (call'3 flag_ds' _cfε' _6 π'5 nq))

call'6 nq' nq'2 = let
  _11 = nq' - (1::Int)
  π'9 = sel7 (call'8 _11 nq'2)
  _19 = sel2 (call'8 _11 nq'2)
  _13 = sel1 (call'8 _11 nq'2)
  _17 = sel14 (call'8 _11 nq'2)
  π'11 = sel16 (call'8 _11 nq'2)
  _12 = sel6 (call'8 _11 nq'2)
  π'7 = sel11 (call'8 _11 nq'2)
  t'3 = sel15 (call'8 _11 nq'2)
  t'2 = sel8 (call'8 _11 nq'2)
  π'10 = sel3 (call'8 _11 nq'2)
  π'8 = sel9 (call'8 _11 nq'2)
  t' = sel10 (call'8 _11 nq'2)
  _16 = sel18 (call'8 _11 nq'2)
  π'12 = sel17 (call'8 _11 nq'2)
  _14 = sel5 (call'8 _11 nq'2)
  _15 = sel4 (call'8 _11 nq'2)
  _18 = sel13 (call'8 _11 nq'2)
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) (sel3 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) _15 (sel21 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (nq' == (0::Int)) (sel7 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) _13 (sel15 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel22 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel12 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel10 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel25 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) _19 _14 (sel29 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel4 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel20 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) t' (sel17 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel2 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel23 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel8 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel5 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel26 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel27 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel18 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel19 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel6 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel9 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel16 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel11 (call'7 π'7 π'8 _12 _13 π'9 _14 t' π'10 π'11 _15 _16 _17 t'2 π'12 _18 _19 t'3 nq'2)) (sel12 (call'8 _11 nq'2))

main = Criterion.Main.defaultMain ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "main"#) $ Criterion.Measurement.Types.whnf lam'3 (8::Int)) : [])

call'9 flag_ds'' _20 _21 _22 _23 _24 ψ'4 π'13 _25 _cfε'5 π'14 π'15 π'16 π'17 π'18 _cfε'6 _26 _27 _28 nq'3 _29 π'19 = let
  _30 = (call'4 flag_ds'' _27 _28 π'13 _cfε'6 _25 π'19 _29 π'16 π'18 π'15 (1::Int) _20)
  _33 = case sel3 (call'2 True flag_ds'' (let (:) _ arg = undefined in arg) ψ'4 _22 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'18 π'17 _cfε'6 _26 _27 _28 _29 nq'3 π'19) of { (:) ρ'14 ρ'15 -> sel1 (call'2 True flag_ds'' (let (:) _ arg = undefined in arg) ψ'4 _22 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'18 π'17 _cfε'6 _26 _27 _28 _29 nq'3 π'19); [] -> sel4 (call'2 True flag_ds'' (let (:) _ arg = undefined in arg) ψ'4 _22 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'18 π'17 _cfε'6 _26 _27 _28 _29 nq'3 π'19) }
  _31 = _20 : π'14
  _32 = _31 : _33
  ψ'5 = case _30 of { True -> _32; False -> sel3 (call'5 True flag_ds'' (let (:) _ arg = undefined in arg) _22 _23 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'17 π'18 _cfε'6 _26 _27 _28 nq'3 _29 π'19) }
  ψ'6 = case π'15 of { (:) ρ'12 ρ'13 -> ψ'5; [] -> _32 }
  in (,,,,,,,,,,,,,,,,,,,) (sel1 (call'3 flag_ds'' _cfε'5 _26 π'17 nq'3)) (sel2 (call'2 True flag_ds'' (let (:) _ arg = undefined in arg) ψ'4 _22 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'18 π'17 _cfε'6 _26 _27 _28 _29 nq'3 π'19)) _30 (sel7 (call'3 flag_ds'' _cfε'5 _26 π'17 nq'3)) (sel5 (call'3 flag_ds'' _cfε'5 _26 π'17 nq'3)) (sel4 (call'3 flag_ds'' _cfε'5 _26 π'17 nq'3)) _31 (sel6 (call'5 True flag_ds'' (let (:) _ arg = undefined in arg) _22 _23 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'17 π'18 _cfε'6 _26 _27 _28 nq'3 _29 π'19)) (case _24 of { True -> sel2 (call'3 flag_ds'' _cfε'5 _26 π'17 nq'3); False -> (case flag_ds'' of { True -> (case _26 of { True -> _32; False -> (case _29 of { True -> (case π'16 of { (:) ρ'8 ρ'9 -> ψ'5; [] -> _32 }); False -> (case _27 of { True -> ψ'5; False -> ψ'6 }) }) }); False -> (case _cfε'6 of { (:) ρ'10 ρ'11 -> ψ'5; [] -> _32 }) }) }) (sel6 (call'3 flag_ds'' _cfε'5 _26 π'17 nq'3)) (sel1 (call'5 True flag_ds'' (let (:) _ arg = undefined in arg) _22 _23 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'17 π'18 _cfε'6 _26 _27 _28 nq'3 _29 π'19)) (sel4 (call'5 True flag_ds'' (let (:) _ arg = undefined in arg) _22 _23 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'17 π'18 _cfε'6 _26 _27 _28 nq'3 _29 π'19)) _33 ψ'4 (sel6 (call'2 True flag_ds'' (let (:) _ arg = undefined in arg) ψ'4 _22 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'18 π'17 _cfε'6 _26 _27 _28 _29 nq'3 π'19)) (sel8 (call'3 flag_ds'' _cfε'5 _26 π'17 nq'3)) (sel7 (call'5 True flag_ds'' (let (:) _ arg = undefined in arg) _22 _23 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'17 π'18 _cfε'6 _26 _27 _28 nq'3 _29 π'19)) (sel3 (call'3 flag_ds'' _cfε'5 _26 π'17 nq'3)) (sel2 (call'5 True flag_ds'' (let (:) _ arg = undefined in arg) _22 _23 _21 π'13 _25 _cfε'5 π'14 π'15 π'16 π'17 π'18 _cfε'6 _26 _27 _28 nq'3 _29 π'19)) ψ'6

call'10 flag_ds _34 t'4 π'20 _35 π'21 π'22 π'23 π'24 π'25 _36 _cfε'7 _37 π'26 t'5 _38 π'27 π'28 π'29 ψ'7 π'30 _39 _40 π'31 _41 _42 π'32 = 
  let _43 = (call'10 False undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined (case flag_ds of { True -> (case _36 of { True -> (case _39 of { True -> (let (:) _ arg = π'23 in arg); False -> (case _36 of { True -> (case ψ'7 of { (:) ρ'16 ρ'17 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }); False -> (case _35 of { True -> (case π'25 of { (:) ρ'18 ρ'19 -> (case _41 of { True -> (case ψ'7 of { (:) ρ'20 ρ'21 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }); False -> (let (:) _ arg = π'29 in arg) }); [] -> (case ψ'7 of { (:) ρ'22 ρ'23 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }) }); False -> (case _37 of { (:) ρ'24 ρ'25 -> (case _34 of { True -> (case π'21 of { (:) ρ'26 ρ'27 -> (case _41 of { True -> (case ψ'7 of { (:) ρ'28 ρ'29 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }); False -> (let (:) _ arg = π'24 in arg) }); [] -> (case ψ'7 of { (:) ρ'30 ρ'31 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }) }); False -> (let (:) _ arg = π'32 in arg) }); [] -> (case π'28 of { (:) ρ'32 ρ'33 -> (case _41 of { True -> (case ψ'7 of { (:) ρ'34 ρ'35 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }); False -> (let (:) _ arg = π'22 in arg) }); [] -> (case ψ'7 of { (:) ρ'36 ρ'37 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }) }) }) }) }) }); False -> (case _39 of { True -> (let (:) _ arg = π'26 in arg); False -> (case _36 of { True -> (case ψ'7 of { (:) ρ'38 ρ'39 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }); False -> (case _35 of { True -> (case π'25 of { (:) ρ'40 ρ'41 -> (case _41 of { True -> (case ψ'7 of { (:) ρ'42 ρ'43 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }); False -> (let (:) _ arg = π'31 in arg) }); [] -> (case ψ'7 of { (:) ρ'44 ρ'45 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }) }); False -> (case _37 of { (:) ρ'46 ρ'47 -> (case _34 of { True -> (case π'21 of { (:) ρ'48 ρ'49 -> (case _41 of { True -> (case ψ'7 of { (:) ρ'50 ρ'51 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }); False -> (let (:) _ arg = π'30 in arg) }); [] -> (case ψ'7 of { (:) ρ'52 ρ'53 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }) }); False -> (let (:) _ arg = π'20 in arg) }); [] -> (case π'28 of { (:) ρ'54 ρ'55 -> (case _41 of { True -> (case ψ'7 of { (:) ρ'56 ρ'57 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }); False -> (let (:) _ arg = π'27 in arg) }); [] -> (case ψ'7 of { (:) ρ'58 ρ'59 -> (let (:) _ arg = t'4 in arg); [] -> (let (:) _ arg = t'5 in arg) }) }) }) }) }) }) }); False -> (let (:) _ arg = _cfε'7 in arg) }) undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined (let (:) _ arg = undefined in arg) undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg)) + (1::Int) in
  case flag_ds of { True -> (case _38 of { True -> (0::Int); False -> (case _36 of { True -> (case _39 of { True -> (case π'23 of { (:) ρ'60 ρ'61 -> _43; [] -> (0::Int) }); False -> (case _36 of { True -> (case _42 of { (:) ρ'62 ρ'63 -> _43; [] -> (0::Int) }); False -> (case _35 of { True -> (case π'25 of { (:) ρ'64 ρ'65 -> (case _41 of { True -> (case _42 of { (:) ρ'66 ρ'67 -> _43; [] -> (0::Int) }); False -> (case π'29 of { (:) ρ'68 ρ'69 -> _43; [] -> (0::Int) }) }); [] -> (case _42 of { (:) ρ'70 ρ'71 -> _43; [] -> (0::Int) }) }); False -> (case _37 of { (:) ρ'72 ρ'73 -> (case _34 of { True -> (case _40 of { (:) ρ'74 ρ'75 -> (case _41 of { True -> (case _42 of { (:) ρ'76 ρ'77 -> _43; [] -> (0::Int) }); False -> (case π'24 of { (:) ρ'78 ρ'79 -> _43; [] -> (0::Int) }) }); [] -> (case _42 of { (:) ρ'80 ρ'81 -> _43; [] -> (0::Int) }) }); False -> (case π'32 of { (:) ρ'82 ρ'83 -> _43; [] -> (0::Int) }) }); [] -> (case π'28 of { (:) ρ'84 ρ'85 -> (case _41 of { True -> (case _42 of { (:) ρ'86 ρ'87 -> _43; [] -> (0::Int) }); False -> (case π'22 of { (:) ρ'88 ρ'89 -> _43; [] -> (0::Int) }) }); [] -> (case _42 of { (:) ρ'90 ρ'91 -> _43; [] -> (0::Int) }) }) }) }) }) }); False -> (case _39 of { True -> (case π'26 of { (:) ρ'92 ρ'93 -> _43; [] -> (0::Int) }); False -> (case _36 of { True -> (case _42 of { (:) ρ'94 ρ'95 -> _43; [] -> (0::Int) }); False -> (case _35 of { True -> (case π'25 of { (:) ρ'96 ρ'97 -> (case _41 of { True -> (case _42 of { (:) ρ'98 ρ'99 -> _43; [] -> (0::Int) }); False -> (case π'31 of { (:) ρ'100 ρ'101 -> _43; [] -> (0::Int) }) }); [] -> (case _42 of { (:) ρ'102 ρ'103 -> _43; [] -> (0::Int) }) }); False -> (case _37 of { (:) ρ'104 ρ'105 -> (case _34 of { True -> (case _40 of { (:) ρ'106 ρ'107 -> (case _41 of { True -> (case _42 of { (:) ρ'108 ρ'109 -> _43; [] -> (0::Int) }); False -> (case π'30 of { (:) ρ'110 ρ'111 -> _43; [] -> (0::Int) }) }); [] -> (case _42 of { (:) ρ'112 ρ'113 -> _43; [] -> (0::Int) }) }); False -> (case π'20 of { (:) ρ'114 ρ'115 -> _43; [] -> (0::Int) }) }); [] -> (case π'28 of { (:) ρ'116 ρ'117 -> (case _41 of { True -> (case _42 of { (:) ρ'118 ρ'119 -> _43; [] -> (0::Int) }); False -> (case π'27 of { (:) ρ'120 ρ'121 -> _43; [] -> (0::Int) }) }); [] -> (case _42 of { (:) ρ'122 ρ'123 -> _43; [] -> (0::Int) }) }) }) }) }) }) }) }); False -> (case _cfε'7 of { (:) ρ'124 ρ'125 -> _43; [] -> (0::Int) }) }

call'8 _44 nq'4 = let
  _45 = _44 - (1::Int)
  _46 = sel6 (call'8 _45 nq'4)
  π'37 = sel16 (call'8 _45 nq'4)
  _53 = sel2 (call'8 _45 nq'4)
  _51 = sel14 (call'8 _45 nq'4)
  π'35 = sel7 (call'8 _45 nq'4)
  _52 = sel13 (call'8 _45 nq'4)
  _49 = sel4 (call'8 _45 nq'4)
  _50 = sel18 (call'8 _45 nq'4)
  _48 = sel5 (call'8 _45 nq'4)
  _47 = sel1 (call'8 _45 nq'4)
  π'38 = sel17 (call'8 _45 nq'4)
  π'36 = sel3 (call'8 _45 nq'4)
  π'34 = sel9 (call'8 _45 nq'4)
  t'7 = sel8 (call'8 _45 nq'4)
  π'33 = sel11 (call'8 _45 nq'4)
  t'6 = sel10 (call'8 _45 nq'4)
  t'8 = sel15 (call'8 _45 nq'4)
  in (,,,,,,,,,,,,,,,,,) (sel17 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) _49 (sel21 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (_44 == (0::Int)) (sel28 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) _47 (sel24 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel22 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel26 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel30 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel13 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel14 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) _53 (sel9 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel11 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel20 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel1 (call'7 π'33 π'34 _46 _47 π'35 _48 t'6 π'36 π'37 _49 _50 _51 t'7 π'38 _52 _53 t'8 nq'4)) (sel12 (call'8 _45 nq'4))

call'3 flag_ds''2 _cfε'8 _54 π'39 nq'5 = let
  π'40 = case undefined of { True -> undefined; False -> (let (:) arg _ = undefined in arg) }
  π'41 = case undefined of { True -> (case undefined of { (:) ρ'130 ρ'131 -> π'40; [] -> undefined }); False -> (case undefined of { (:) ρ'132 ρ'133 -> (case undefined of { True -> (case undefined of { (:) ρ'134 ρ'135 -> π'40; [] -> undefined }); False -> (case undefined of { (:) ρ'136 ρ'137 -> π'40; [] -> undefined }) }); [] -> (case undefined of { (:) ρ'138 ρ'139 -> π'40; [] -> undefined }) }) }
  _58 = sel1 (call'11 nq'5 (1::Int))
  _56 = sel3 (call'11 nq'5 (1::Int))
  _55 = sel6 (call'11 nq'5 (1::Int))
  _cfε'10 = case flag_ds''2 of { True -> (let (:) arg _ = π'39 in arg); False -> (let (:) arg _ = _cfε'8 in arg) }
  ψ'8 = sel5 (call'11 nq'5 (1::Int))
  _57 = sel2 (call'11 nq'5 (1::Int))
  _cfε'9 = case flag_ds''2 of { True -> (let (:) _ arg = π'39 in arg); False -> (let (:) _ arg = _cfε'8 in arg) }
  t'9 = sel9 (call'9 False (1::Int) _55 _56 _57 _58 ψ'8 undefined undefined _cfε'9 _cfε'10 π'41 (let (:) arg _ = undefined in arg) (let (:) _ arg = undefined in arg) undefined _cfε'10 undefined undefined undefined nq'5 undefined undefined)
  π'42 = case _54 of { True -> []; False -> (case π'39 of { (:) ρ'128 ρ'129 -> t'9; [] -> [] }) }
  _ccε' = case _cfε'8 of { (:) ρ'126 ρ'127 -> t'9; [] -> [] }
  in (,,,,,,,) (sel4 (call'9 False (1::Int) _55 _56 _57 _58 ψ'8 undefined undefined _cfε'9 _cfε'10 π'41 (let (:) arg _ = undefined in arg) (let (:) _ arg = undefined in arg) undefined _cfε'10 undefined undefined undefined nq'5 undefined undefined)) (case flag_ds''2 of { True -> π'42; False -> _ccε' }) (let (:) arg _ = π'39 in arg) (sel3 (call'9 False (1::Int) _55 _56 _57 _58 ψ'8 undefined undefined _cfε'9 _cfε'10 π'41 (let (:) arg _ = undefined in arg) (let (:) _ arg = undefined in arg) undefined _cfε'10 undefined undefined undefined nq'5 undefined undefined)) _58 π'42 _ccε' (sel19 (call'9 False (1::Int) _55 _56 _57 _58 ψ'8 undefined undefined _cfε'9 _cfε'10 π'41 (let (:) arg _ = undefined in arg) (let (:) _ arg = undefined in arg) undefined _cfε'10 undefined undefined undefined nq'5 undefined undefined))

enumFromTo_mine = lam'

call'12 from' to' = let
  _59 = from' + (1::Int)
  ψ'9 = sel3 (call' _59 to')
  _60 = from' : ψ'9
  _61 = from' > to'
  in (,,,,,,) ψ'9 _60 (sel1 (call' _59 to')) _61 (case _61 of { True -> []; False -> _60 }) (sel2 (call' _59 to')) _59

call'13 _62 = _62

lam'4 = \nq'6 -> (call'14 (sel15 (call'6 nq'6 nq'6)) (sel6 (call'6 nq'6 nq'6)) (sel2 (call'6 nq'6 nq'6)) (sel14 (call'6 nq'6 nq'6)) (sel25 (call'6 nq'6 nq'6)) (sel27 (call'6 nq'6 nq'6)) (sel26 (call'6 nq'6 nq'6)) (sel4 (call'6 nq'6 nq'6)) (sel7 (call'6 nq'6 nq'6)) (sel5 (call'6 nq'6 nq'6)) (sel13 (call'6 nq'6 nq'6)) (sel23 (call'6 nq'6 nq'6)) (sel8 (call'6 nq'6 nq'6)) (sel19 (call'6 nq'6 nq'6)) (sel16 (call'6 nq'6 nq'6)) (sel18 (call'6 nq'6 nq'6)) (sel3 (call'6 nq'6 nq'6)) (sel30 (call'6 nq'6 nq'6)) (sel31 (call'6 nq'6 nq'6)) (sel9 (call'6 nq'6 nq'6)) (sel24 (call'6 nq'6 nq'6)) (sel1 (call'6 nq'6 nq'6)) (sel21 (call'6 nq'6 nq'6)) (sel12 (call'6 nq'6 nq'6)) (sel28 (call'6 nq'6 nq'6)) (sel22 (call'6 nq'6 nq'6)) (sel10 (call'6 nq'6 nq'6)) (sel11 (call'6 nq'6 nq'6)) (sel20 (call'6 nq'6 nq'6)) (sel17 (call'6 nq'6 nq'6)) (sel29 (call'6 nq'6 nq'6)))

call'15 x = x

call'14 _63 _64 _65 π'43 _ccε'2 t'10 _cfε'11 _66 _67 π'44 _68 π'45 t'11 _69 π'46 _70 π'47 t'12 _71 π'48 t'13 t'14 ψ'10 _72 _73 π'49 t'15 π'50 _ccε'3 t'16 _74 = let
  _75 = (call'10 True _72 t'14 (let (:) _ arg = π'44 in arg) _64 π'47 (let (:) _ arg = t'12 in arg) (let (:) _ arg = t'11 in arg) (let (:) _ arg = t'12 in arg) π'46 _65 (let (:) _ arg = undefined in arg) _71 (let (:) _ arg = t'11 in arg) t'15 _66 (let (:) _ arg = t'12 in arg) π'45 (let (:) _ arg = t'12 in arg) ψ'10 (let (:) _ arg = t'12 in arg) _70 _68 (let (:) _ arg = t'12 in arg) _73 _69 (let (:) _ arg = π'44 in arg)) + (1::Int)
  ψ'12 = case _73 of { True -> _75; False -> (case π'49 of { (:) ρ'156 ρ'157 -> (case _65 of { True -> _75; False -> (case _64 of { True -> (case π'46 of { (:) ρ'158 ρ'159 -> (case _63 of { True -> _75; False -> (case _ccε'2 of { (:) ρ'160 ρ'161 -> _75; [] -> (0::Int) }) }); [] -> _75 }); False -> (case _71 of { (:) ρ'162 ρ'163 -> (case _72 of { True -> (case π'47 of { (:) ρ'164 ρ'165 -> (case _63 of { True -> _75; False -> (case _ccε'2 of { (:) ρ'166 ρ'167 -> _75; [] -> (0::Int) }) }); [] -> _75 }); False -> (case π'48 of { (:) ρ'168 ρ'169 -> _75; [] -> (0::Int) }) }); [] -> (case π'45 of { (:) ρ'170 ρ'171 -> (case _63 of { True -> _75; False -> (case _ccε'2 of { (:) ρ'172 ρ'173 -> _75; [] -> (0::Int) }) }); [] -> _75 }) }) }) }); [] -> (case t'10 of { (:) ρ'174 ρ'175 -> _75; [] -> (0::Int) }) }) }
  ψ'11 = case _70 of { True -> (case _65 of { True -> (0::Int); False -> (case π'43 of { (:) ρ'142 ρ'143 -> (case _67 of { True -> (case _ccε'3 of { (:) ρ'144 ρ'145 -> _75; [] -> (0::Int) }); False -> (case _cfε'11 of { (:) ρ'146 ρ'147 -> (case _74 of { True -> _75; False -> (case t'13 of { (:) ρ'148 ρ'149 -> _75; [] -> (0::Int) }) }); [] -> _75 }) }); [] -> (0::Int) }) }); False -> (case _65 of { True -> _75; False -> (case _64 of { True -> (case π'46 of { (:) ρ'150 ρ'151 -> ψ'12; [] -> _75 }); False -> (case _72 of { True -> (case _68 of { (:) ρ'152 ρ'153 -> ψ'12; [] -> _75 }); False -> (case π'50 of { (:) ρ'154 ρ'155 -> ψ'12; [] -> _75 }) }) }) }) }
  in case _66 of { True -> _75; False -> (case _65 of { True -> ψ'11; False -> (case t'16 of { (:) ρ'140 ρ'141 -> ψ'11; [] -> (0::Int) }) }) }

lam = \ds -> case ds of { (:) ρ'176 ρ'177 -> (call'10 False undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined ρ'177 undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined (let (:) _ arg = undefined in arg) undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg)) + (1::Int); [] -> (0::Int) }

call'16 flag_ds'_ds'2 π'51 π'52 π'53 π'54 π'55 _76 _cfε'12 _77 _78 d x' = let
  _cfε'13 = case flag_ds'_ds'2 of { True -> (case _76 of { True -> (let (:) arg _ = π'54 in arg); False -> (case _77 of { True -> (let (:) arg _ = π'53 in arg); False -> (let (:) arg _ = π'51 in arg) }) }); False -> (let (:) arg _ = _cfε'12 in arg) }
  _79 = (x' /= _cfε'13) && ((x' /= (_cfε'13 + d)) && ((x' /= (_cfε'13 - d)) && (call'16 False (let (:) _ arg = undefined in arg) undefined undefined (let (:) _ arg = undefined in arg) undefined undefined (case flag_ds'_ds'2 of { True -> (case _76 of { True -> (let (:) _ arg = π'54 in arg); False -> (case _77 of { True -> (case _78 of { True -> (let (:) _ arg = π'55 in arg); False -> (let (:) _ arg = π'52 in arg) }); False -> (let (:) _ arg = π'51 in arg) }) }); False -> (let (:) _ arg = _cfε'12 in arg) }) undefined undefined (d + (1::Int)) x')))
  in case flag_ds'_ds'2 of { True -> (case _76 of { True -> (case π'54 of { (:) ρ'178 ρ'179 -> _79; [] -> True }); False -> (case _77 of { True -> (case _77 of { True -> True; False -> (case _78 of { True -> (case π'55 of { (:) ρ'180 ρ'181 -> _79; [] -> True }); False -> (case π'52 of { (:) ρ'182 ρ'183 -> _79; [] -> True }) }) }); False -> (case π'51 of { (:) ρ'184 ρ'185 -> _79; [] -> True }) }) }); False -> (case _cfε'12 of { (:) ρ'186 ρ'187 -> _79; [] -> True }) }

lam'2 from'2 = \to'2 -> sel5 (call'12 from'2 to'2)

call'4 flag_ds''3 _80 _81 π'56 _cfε'14 _82 π'57 _83 π'58 π'59 π'60 d' x'2 = 
  let π'61 = case flag_ds''3 of { True -> (case _83 of { True -> (let (:) arg _ = π'58 in arg); False -> (case _80 of { True -> (let (:) arg _ = _81 in arg); False -> (let (:) arg _ = π'60 in arg) }) }); False -> (let (:) arg _ = _cfε'14 in arg) } in
  (x'2 /= π'61) && ((x'2 /= (π'61 + d')) && ((x'2 /= (π'61 - d')) && (call'16 flag_ds''3 (let (:) _ arg = π'60 in arg) π'59 π'56 (let (:) _ arg = π'58 in arg) π'57 _83 (let (:) _ arg = _cfε'14 in arg) _80 _82 (d' + (1::Int)) x'2)))

call'17 π'62 = π'62

call'7 π'63 π'64 _84 _85 π'65 _86 t'17 π'66 π'67 _87 _88 _89 t'18 π'68 _90 _91 t'19 nq'7 = let
  π'69 = case _89 of { True -> _86; False -> (let (:) arg _ = t'19 in arg) }
  π'71 = case _84 of { True -> (case π'67 of { (:) ρ'190 ρ'191 -> π'69; [] -> _86 }); False -> (case _88 of { (:) ρ'192 ρ'193 -> (case _90 of { True -> (case π'66 of { (:) ρ'194 ρ'195 -> π'69; [] -> _86 }); False -> (case π'65 of { (:) ρ'196 ρ'197 -> π'69; [] -> _86 }) }); [] -> (case π'64 of { (:) ρ'198 ρ'199 -> π'69; [] -> _86 }) }) }
  π'72 = case _91 of { True -> _86; False -> π'71 }
  π'70 = case _87 of { True -> []; False -> (case _85 of { True -> (let (:) arg _ = t'18 in arg); False -> π'72 }) }
  _95 = sel1 (call'11 nq'7 (1::Int))
  _93 = sel3 (call'11 nq'7 (1::Int))
  _94 = sel2 (call'11 nq'7 (1::Int))
  _92 = sel6 (call'11 nq'7 (1::Int))
  ψ'13 = sel5 (call'11 nq'7 (1::Int))
  t'20 = sel9 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) π'70 (sel13 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel2 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel11 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel14 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel17 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel20 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) ψ'13 (sel3 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel15 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel19 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel8 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) π'72 (sel4 (call'11 nq'7 (1::Int))) (sel5 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel6 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) _95 (sel12 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel18 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (let (:) arg _ = t'18 in arg) _86 (sel10 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel1 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) π'71 π'71 (let (:) arg _ = t'18 in arg) (sel16 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (sel7 (call'9 True (1::Int) _92 _93 _94 _95 ψ'13 π'68 _84 (let (:) _ arg = undefined in arg) π'70 π'71 (let (:) arg _ = t'18 in arg) (let (:) _ arg = t'17 in arg) π'63 (let (:) arg _ = undefined in arg) _87 _91 _86 nq'7 _85 π'67)) (let (:) _ arg = t'17 in arg) (case _87 of { True -> t'20; False -> (case t'17 of { (:) ρ'188 ρ'189 -> t'20; [] -> [] }) })

nsoln = lam'4

call'5 flag_ds'3' flag_ds''4 _cfε'15 _96 _97 _98 π'73 _99 _cfε'16 π'74 π'75 π'76 π'77 π'78 _cfε'17 _100 _101 _102 nq'8 _103 π'79 = let
  _cfε'18 = case flag_ds'3' of { True -> _98; False -> (let (:) _ arg = _cfε'15 in arg) }
  _cfε'19 = case flag_ds'3' of { True -> _96; False -> (let (:) arg _ = _cfε'15 in arg) }
  _105 = (_cfε'19 : π'74) : sel5 (call'2 False flag_ds''4 _cfε'18 undefined undefined undefined π'73 _99 _cfε'16 π'74 π'75 π'76 π'78 π'77 _cfε'17 _100 _101 _102 _103 nq'8 π'79)
  _ccε'4 = sel5 (call'5 False flag_ds''4 _cfε'18 undefined undefined undefined π'73 _99 _cfε'16 π'74 π'75 π'76 π'77 π'78 _cfε'17 _100 _101 _102 nq'8 _103 π'79)
  _104 = (call'4 flag_ds''4 _101 _102 π'73 _cfε'17 _99 π'79 _103 π'76 π'78 π'75 (1::Int) _cfε'19)
  ψ'17 = case _104 of { True -> _105; False -> _ccε'4 }
  ψ'16 = case π'75 of { (:) ρ'206 ρ'207 -> ψ'17; [] -> _105 }
  ψ'15 = case flag_ds''4 of { True -> (case _100 of { True -> _105; False -> (case _103 of { True -> (case π'76 of { (:) ρ'202 ρ'203 -> ψ'17; [] -> _105 }); False -> (case _101 of { True -> ψ'17; False -> ψ'16 }) }) }); False -> (case _cfε'17 of { (:) ρ'204 ρ'205 -> ψ'17; [] -> _105 }) }
  _ccε'5 = sel2 (call'3 flag_ds''4 _cfε'16 _100 π'77 nq'8)
  ψ'14 = case _97 of { True -> _ccε'5; False -> ψ'15 }
  in (,,,,,,) _104 ψ'14 ψ'14 _ccε'4 (case _cfε'15 of { (:) ρ'200 ρ'201 -> ψ'15; [] -> _ccε'5 }) ψ'16 (sel6 (call'3 flag_ds''4 _cfε'16 _100 π'77 nq'8))

call'11 nq'9 from'3 = (,,,,,) (sel4 (call'12 from'3 nq'9)) (sel6 (call'12 from'3 nq'9)) (sel7 (call'12 from'3 nq'9)) (sel2 (call'12 from'3 nq'9)) (sel1 (call'12 from'3 nq'9)) (sel3 (call'12 from'3 nq'9))

lam'3 = \n -> (call'18 n)

call'19 _106 = _106

call'18 n' = (call'14 (sel15 (call'6 n' n')) (sel6 (call'6 n' n')) (sel2 (call'6 n' n')) (sel14 (call'6 n' n')) (sel25 (call'6 n' n')) (sel27 (call'6 n' n')) (sel26 (call'6 n' n')) (sel4 (call'6 n' n')) (sel7 (call'6 n' n')) (sel5 (call'6 n' n')) (sel13 (call'6 n' n')) (sel23 (call'6 n' n')) (sel8 (call'6 n' n')) (sel19 (call'6 n' n')) (sel16 (call'6 n' n')) (sel18 (call'6 n' n')) (sel3 (call'6 n' n')) (sel30 (call'6 n' n')) (sel31 (call'6 n' n')) (sel9 (call'6 n' n')) (sel24 (call'6 n' n')) (sel1 (call'6 n' n')) (sel21 (call'6 n' n')) (sel12 (call'6 n' n')) (sel28 (call'6 n' n')) (sel22 (call'6 n' n')) (sel10 (call'6 n' n')) (sel11 (call'6 n' n')) (sel20 (call'6 n' n')) (sel17 (call'6 n' n')) (sel29 (call'6 n' n')))
