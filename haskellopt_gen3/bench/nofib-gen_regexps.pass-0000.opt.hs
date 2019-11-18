-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  31
-- Incl. one-shot:   0
-- Case reductions:  32
-- Field reductions: 73
-- Case commutings:  296
-- Total nodes: 7164; Boxes: 3641; Branches: 296
-- Apps: 505; Lams: 29

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main (main,hash,expand,alphabeticRule,constantRule,numericRule) where

import Data.Tuple.Select
import Control.Exception.Base
import Criterion.Main
import Criterion.Measurement.Types
import Data.Foldable
import GHC.Base
import GHC.CString
import GHC.Classes
import GHC.Enum
import GHC.List
import GHC.Num
import GHC.Prim
import GHC.Show
import GHC.Tuple
import GHC.Types

hash = Data.Foldable.foldl' lam (0::Int)

lam' u = \c -> (u * (10::Int)) + (ord c - ord (C# '0'#))

lam'2 = \ds -> case ds of { (:) ρ ρ' -> (case ρ == GHC.List.head (GHC.CString.unpackCString# "<"#) of { True -> sel4 (call ρ'); False -> (case ρ == GHC.List.head (GHC.CString.unpackCString# "["#) of { True -> sel28 (call' False ρ' (let (:) _ arg = undefined in arg) undefined); False -> sel7 (call'2 False undefined undefined ds ds) }) }); [] -> [] : [] }

call'3 π π' = 
  let ret = (call'4 π') in
  case π of { (:) ρ'2 ρ'3 -> sel6 (call'5 π' (sel6 ret) (sel26 ret) (sel24 ret) (sel7 ret) (sel23 ret) (sel20 ret) (sel16 ret) (sel27 ret) (sel11 ret) (sel9 ret) (sel17 ret) (sel10 ret) (sel14 ret) (sel19 ret) (sel33 ret) (sel32 ret) (sel2 ret) (sel4 ret) (sel12 ret) (sel3 ret) (sel28 ret) (sel30 ret) (sel8 ret) (sel18 ret) (sel34 ret) (sel21 ret) (sel25 ret) (sel15 ret) (sel5 ret) (sel22 ret) (sel29 ret) (sel1 ret) (sel13 ret) π' ρ'3 ρ'2); [] -> [] }

constantRule = lam'3

call'6 _0 = case _0 of { (:) ρ'4 ρ'5 -> C# '0'# : (call'7 ρ'5); [] -> [] }

call'8 flag_ds'4 _cfε π'2 _1 π'3 _2 _3 π'4 = let
  ret' = (call'9 π'4 _3)
  t = sel2 (call'10 False (sel1 ret') (sel2 ret') (sel10 ret') (sel14 ret') _3 (sel11 ret') (sel8 ret') (sel9 ret') (sel12 ret') (sel13 ret') (sel7 ret') (sel6 ret') (sel5 ret') (sel4 ret') (sel15 ret') π'4 (sel3 ret') undefined π'4 _3 (case flag_ds'4 of { True -> (case _1 of { True -> (let (:) arg _ = π'3 in arg); False -> (let (:) arg _ = π'2 in arg) }); False -> (let (:) arg _ = _cfε in arg) }) _2 (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) (case flag_ds'4 of { True -> (case _1 of { True -> (let (:) _ arg = π'3 in arg); False -> (let (:) _ arg = π'2 in arg) }); False -> (let (:) _ arg = _cfε in arg) }))
  in case flag_ds'4 of { True -> (case _1 of { True -> (case π'3 of { (:) ρ'6 ρ'7 -> t; [] -> [] }); False -> (case π'2 of { (:) ρ'8 ρ'9 -> t; [] -> [] }) }); False -> (case _cfε of { (:) ρ'10 ρ'11 -> t; [] -> [] }) }

lam'4 = \x -> let
  _5 = GHC.List.span (lam'6 (C# '-'#)) x
  _4 = GHC.List.span (lam'5 (C# '>'#)) (case _5 of { (,) ρ'16 ρ'17 -> (let (:) _ arg = ρ'17 in arg) })
  ret'3 = (call'13 (case _4 of { (,) ρ'14 ρ'15 -> ρ'14 }))
  ret'2 = (call'12 (case _5 of { (,) ρ'12 ρ'13 -> ρ'12 }))
  in sel2 (call'11 (GHC.Enum.enumFromThenTo ret'2 (ret'2 - (1::Int)) ret'3) (ret'2 < ret'3) (GHC.Enum.enumFromTo ret'2 ret'3) (max (Data.Foldable.length (GHC.Show.show ret'2)) (Data.Foldable.length (GHC.Show.show ret'3))) _4 (let (:) _ arg = (let (,) _ arg = _4 in arg) in arg))

call'14 π'5 = let
  ret'4 = (call'2 False undefined undefined π'5 π'5)
  ret'5 = (call' False (let (:) _ arg = π'5 in arg) (let (:) _ arg = undefined in arg) undefined)
  ret'6 = (call (let (:) _ arg = π'5 in arg))
  _7 = (let (:) arg _ = π'5 in arg) == GHC.List.head (GHC.CString.unpackCString# "["#)
  _6 = (let (:) arg _ = π'5 in arg) == GHC.List.head (GHC.CString.unpackCString# "<"#)
  in (,,,,,,,,,,,,,,,,,,,) (sel39 ret'5) (sel18 ret'5) (sel1 ret'6) (sel43 ret'5) (sel2 ret'4) (sel5 ret'4) (sel9 ret'4) (case π'5 of { (:) ρ'18 ρ'19 -> (case _6 of { True -> sel4 ret'6; False -> (case _7 of { True -> sel28 ret'5; False -> sel7 ret'4 }) }); [] -> [] : [] }) (sel1 ret'4) (sel8 ret'4) (sel3 ret'4) (sel10 ret'4) (sel6 ret'4) (sel5 ret'6) (sel40 ret'5) (sel3 ret'6) _7 (sel33 ret'5) _6 (sel2 ret'6)

lam = \acc -> (lam'7 acc)

call'15 π'6 = let
  ret'6 = (call (let (:) _ arg = π'6 in arg))
  ret'5 = (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)
  ret'4 = (call'2 False undefined undefined π'6 π'6)
  _9 = (let (:) arg _ = π'6 in arg) == GHC.List.head (GHC.CString.unpackCString# "["#)
  _8 = (let (:) arg _ = π'6 in arg) == GHC.List.head (GHC.CString.unpackCString# "<"#)
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) (sel34 ret'5) (sel25 ret'5) (sel18 ret'5) (sel43 ret'5) (sel30 ret'5) (sel21 ret'5) (sel24 ret'5) (sel8 ret'5) (sel14 ret'5) (sel3 ret'6) (sel27 ret'5) (sel36 ret'5) (sel39 ret'5) (sel1 ret'6) (sel5 ret'4) (sel16 ret'5) (sel13 ret'5) (case π'6 of { (:) ρ'20 ρ'21 -> (case _8 of { True -> sel4 ret'6; False -> (case _9 of { True -> sel28 ret'5; False -> sel7 ret'4 }) }); [] -> [] : [] }) (sel4 ret'4) (sel22 ret'5) (sel5 ret'6) (sel35 ret'5) (sel40 ret'5) (sel12 ret'5) _9 (sel1 ret'5) (sel11 ret'5) (sel15 ret'5) (sel33 ret'5) _8 (sel2 ret'6) (sel20 ret'5) (sel29 ret'5)

call'11 _10 _11 _12 _13 _14 π'7 = let
  ret' = (call'9 π'7 _14)
  ret'7 = (call'10 True (sel1 ret') (sel2 ret') (sel10 ret') (sel14 ret') _14 (sel11 ret') (sel8 ret') (sel9 ret') (sel12 ret') (sel13 ret') (sel7 ret') (sel6 ret') (sel5 ret') (sel4 ret') (sel15 ret') π'7 (sel3 ret') _11 π'7 _14 (case _11 of { True -> (let (:) arg _ = _12 in arg); False -> (let (:) arg _ = _10 in arg) }) _13 (let (:) _ arg = _12 in arg) (let (:) _ arg = _10 in arg) (let (:) _ arg = undefined in arg))
  t' = sel2 ret'7
  in (,) (sel1 ret'7) (case _11 of { True -> (case _12 of { (:) ρ'22 ρ'23 -> t'; [] -> [] }); False -> (case _10 of { (:) ρ'24 ρ'25 -> t'; [] -> [] }) })

call'10 flag_ds'4' ψ t'2 _15 _16 _17 _18 t'3 t'4 _19 _20 t'5 _21 _22 t'6 _23 π'8 _24 _25 π'9 _26 π'10 _27 π'11 π'12 _cfε' = let
  ret'8 = (call'8 flag_ds'4' _cfε' π'12 _25 π'11 _27 _26 π'9)
  π'13 = case _17 of { (,) ρ'56 ρ'57 -> (case π'8 of { (:) ρ'58 ρ'59 -> (case _16 of { True -> (case _24 of { True -> (let (:) _ arg = t'3 in arg); False -> (let (:) _ arg = t'3 in arg) }); False -> (case _18 of { True -> (case _22 of { True -> (let (:) _ arg = t'2 in arg); False -> (let (:) _ arg = t'4 in arg) }); False -> (let (:) _ arg = t'5 in arg) }) }); [] -> [] }) }
  ret'9 = (call'17 flag_ds'4' π'13 _25 π'9 _26 π'10 _27 π'11 π'12 _cfε')
  _28 = ((call'16 (GHC.Show.show π'10) _27) ++ (case _17 of { (,) ρ'48 ρ'49 -> (case π'8 of { (:) ρ'50 ρ'51 -> (case _16 of { True -> (case _24 of { True -> (let (:) arg _ = t'3 in arg); False -> (let (:) arg _ = t'3 in arg) }); False -> (case _18 of { True -> (case _22 of { True -> (case t'6 of { (:) ρ'52 ρ'53 -> _19; [] -> (let (:) arg _ = ψ in arg) }); False -> (let (:) arg _ = t'4 in arg) }); False -> (let (:) arg _ = t'5 in arg) }) }); [] -> [] }) })) : (case π'13 of { (:) ρ'54 ρ'55 -> sel2 ret'9; [] -> sel1 ret'9 })
  t'7 = case _17 of { (,) ρ'26 ρ'27 -> (case π'8 of { (:) ρ'28 ρ'29 -> (case _16 of { True -> (case _24 of { True -> (case _23 of { (:) ρ'30 ρ'31 -> (case t'3 of { (:) ρ'32 ρ'33 -> _28; [] -> ret'8 }); [] -> ret'8 }); False -> (case _15 of { (:) ρ'34 ρ'35 -> (case t'3 of { (:) ρ'36 ρ'37 -> _28; [] -> ret'8 }); [] -> ret'8 }) }); False -> (case _18 of { True -> (case _22 of { True -> (case _21 of { (:) ρ'38 ρ'39 -> (case t'2 of { (:) ρ'40 ρ'41 -> _28; [] -> ret'8 }); [] -> ret'8 }); False -> (case _20 of { (:) ρ'42 ρ'43 -> (case t'4 of { (:) ρ'44 ρ'45 -> _28; [] -> ret'8 }); [] -> ret'8 }) }); False -> (case t'5 of { (:) ρ'46 ρ'47 -> _28; [] -> ret'8 }) }) }); [] -> _28 }) }
  in (,) t'7 t'7

call'5 π'14 _29 _30 ψ' π'15 π'16 ψ'2 _31 t'8 _32 _33 t'9 π'17 π'18 π'19 _34 _35 _36 _cfε'2 _ccε _37 t'10 _38 t'11 t'12 _39 _40 _41 t'13 _42 _ccε' _43 ψ'3 _44 π'20 π'21 π'22 = let
  ret'10 = (call'18 True _41 _30 _42 (let (:) _ arg = undefined in arg) _37 π'18 π'14 _31 π'16 _44 (let (:) _ arg = ψ'3 in arg) (let (:) _ arg = t'8 in arg) t'9 _cfε'2 π'15 _34 _33 π'17 (let (:) _ arg = t'10 in arg) (let (:) _ arg = t'8 in arg) _29 _43 ψ' _ccε π'19 (let (:) _ arg = t'12 in arg) π'20 π'21 π'22)
  _47 = sel8 ret'10
  _49 = sel4 ret'10
  _46 = case sel3 ret'10 of { (:) ρ'86 ρ'87 -> (case sel7 ret'10 of { True -> (case sel5 ret'10 of { True -> (case (let (:) _ arg = t'8 in arg) of { (:) ρ'88 ρ'89 -> _47; [] -> _49 }); False -> (case (let (:) _ arg = t'8 in arg) of { (:) ρ'90 ρ'91 -> _47; [] -> _49 }) }); False -> (case sel2 ret'10 of { True -> (case sel6 ret'10 of { True -> (case sel1 ret'10 of { (:) ρ'92 ρ'93 -> (case _40 of { (:) ρ'94 ρ'95 -> _47; [] -> _49 }); [] -> (case (let (:) _ arg = ψ'3 in arg) of { (:) ρ'96 ρ'97 -> _47; [] -> _49 }) }); False -> (case (let (:) _ arg = t'10 in arg) of { (:) ρ'98 ρ'99 -> _47; [] -> _49 }) }); False -> (case (let (:) _ arg = t'12 in arg) of { (:) ρ'100 ρ'101 -> _47; [] -> _49 }) }) }); [] -> _49 }
  _45 = π'22 : (case π'14 of { (:) ρ'82 ρ'83 -> (case _34 of { True -> (case _31 of { True -> (let (:) arg _ = t'8 in arg); False -> (let (:) arg _ = t'8 in arg) }); False -> (case _43 of { True -> (case _37 of { True -> (case t'9 of { (:) ρ'84 ρ'85 -> _38; [] -> (let (:) arg _ = ψ'3 in arg) }); False -> (let (:) arg _ = t'10 in arg) }); False -> (let (:) arg _ = t'12 in arg) }) }); [] -> [] })
  _48 = _45 : _46
  ret'11 = (call'3 π'21 π'20)
  _ccε'2 = case π'14 of { (:) ρ'60 ρ'61 -> (case _34 of { True -> (case _31 of { True -> (case _39 of { (:) ρ'62 ρ'63 -> (case t'8 of { (:) ρ'64 ρ'65 -> _48; [] -> ret'11 }); [] -> ret'11 }); False -> (case _32 of { (:) ρ'66 ρ'67 -> (case t'8 of { (:) ρ'68 ρ'69 -> _48; [] -> ret'11 }); [] -> ret'11 }) }); False -> (case _43 of { True -> (case _37 of { True -> (case _36 of { (:) ρ'70 ρ'71 -> (case t'13 of { (:) ρ'72 ρ'73 -> _48; [] -> ret'11 }); [] -> ret'11 }); False -> (case _35 of { (:) ρ'74 ρ'75 -> (case t'11 of { (:) ρ'76 ρ'77 -> _48; [] -> (case ψ'2 of { (:) ρ'78 ρ'79 -> _48; [] -> ret'11 }) }); [] -> ret'11 }) }); False -> (case t'12 of { (:) ρ'80 ρ'81 -> _48; [] -> ret'11 }) }) }); [] -> _48 }
  in (,,,,,,,,,,,,) (let (:) _ arg = t'12 in arg) _ccε' (let (:) _ arg = t'10 in arg) (sel9 ret'10) (let (:) _ arg = ψ'3 in arg) _ccε'2 (let (:) _ arg = t'8 in arg) _ccε'2 (let (:) _ arg = t'8 in arg) _45 ret'11 _46 _47

lam'3 = \ds'5 -> 
  let ret'12 = (call'14 (let (:) _ arg = ds'5 in arg)) in
  sel6 (call'19 (sel11 ret'12) (sel13 ret'12) (sel2 ret'12) (sel6 ret'12) (let (:) _ arg = ds'5 in arg) (sel10 ret'12) (sel1 ret'12) (sel4 ret'12) (sel12 ret'12) (sel18 ret'12) (sel19 ret'12) (sel9 ret'12) (sel16 ret'12) (sel8 ret'12) (sel7 ret'12) (sel17 ret'12) (sel14 ret'12) (sel15 ret'12) (sel5 ret'12) (sel3 ret'12) (sel20 ret'12) (let (:) arg _ = ds'5 in arg))

lam'8 = \u' -> (lam' u')

call'13 t'14 = Data.Foldable.foldl lam'8 (0::Int) t'14

call'20 _50 π'23 = let
  ret = (call'4 π'23)
  _ccε'3 = sel22 ret
  _53 = sel3 ret
  _51 = sel33 ret
  _55 = sel21 ret
  _54 = sel29 ret
  _52 = sel16 ret
  ret'13 = (call'5 π'23 (sel6 ret) (sel26 ret) (sel24 ret) (sel7 ret) (sel23 ret) (sel20 ret) _52 (sel27 ret) (sel11 ret) (sel9 ret) (sel17 ret) (sel10 ret) (sel14 ret) (sel19 ret) _51 (sel32 ret) (sel2 ret) (sel4 ret) (sel12 ret) _53 (sel28 ret) (sel30 ret) (sel8 ret) (sel18 ret) (sel34 ret) _55 (sel25 ret) (sel15 ret) (sel5 ret) _ccε'3 _54 (sel1 ret) (sel13 ret) π'23 (let (:) _ arg = _50 in arg) (let (:) arg _ = _50 in arg))
  in (,,,,,,,,,,,,,,,,,,,) (sel10 ret'13) (sel5 ret'13) _51 (sel12 ret'13) (sel7 ret'13) (sel9 ret'13) (sel13 ret'13) (sel3 ret'13) _52 (sel1 ret'13) _53 (case _50 of { (:) ρ'102 ρ'103 -> sel6 ret'13; [] -> [] }) _54 (sel8 ret'13) (sel31 ret) _55 (sel4 ret'13) _ccε'3 (sel2 ret'13) (sel11 ret'13)

call'21 π'24 π'25 = 
  let ret'14 = (call'15 π'25) in
  case π'24 of { (:) ρ'104 ρ'105 -> sel11 (call'22 π'25 (sel2 ret'14) (sel15 ret'14) (sel11 ret'14) (sel28 ret'14) (sel5 ret'14) (sel22 ret'14) (sel33 ret'14) (sel19 ret'14) (sel3 ret'14) (sel6 ret'14) (sel17 ret'14) (sel32 ret'14) (sel12 ret'14) (sel14 ret'14) (sel26 ret'14) (sel1 ret'14) (sel10 ret'14) (sel16 ret'14) (sel4 ret'14) (sel25 ret'14) (sel23 ret'14) (sel29 ret'14) (sel21 ret'14) (sel20 ret'14) (sel27 ret'14) (sel9 ret'14) (sel8 ret'14) (sel31 ret'14) (sel18 ret'14) (sel13 ret'14) (sel24 ret'14) (sel30 ret'14) π'25 ρ'104 ρ'105); [] -> [] }

main = Criterion.Main.defaultMain ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "main"#) $ Criterion.Measurement.Types.whnf lam'9 (GHC.CString.unpackCString# "420"#)) : [])

call'18 flag_ds'3 _56 _57 _58 _cfε'3 _59 π'26 π'27 _60 π'28 _61 π'29 π'30 t'15 _cfε'4 π'31 _62 _63 π'32 π'33 π'34 _64 _65 ψ'4 _ccε'4 π'35 π'36 π'37 π'38 π'39 = let
  ret'11 = (call'3 π'38 π'37)
  _cfε'5 = case flag_ds'3 of { True -> (case _62 of { True -> (case _60 of { True -> (let (:) _ arg = π'30 in arg); False -> (let (:) _ arg = π'34 in arg) }); False -> (case _65 of { True -> (case _59 of { True -> (case t'15 of { (:) ρ'126 ρ'127 -> (case _cfε'4 of { (:) ρ'128 ρ'129 -> (case _63 of { True -> (case _56 of { True -> (case π'28 of { (:) ρ'130 ρ'131 -> (let (:) _ arg = _57 in arg); [] -> (let (:) _ arg = ψ'4 in arg) }); False -> (case π'31 of { (:) ρ'132 ρ'133 -> (let (:) _ arg = _57 in arg); [] -> (let (:) _ arg = ψ'4 in arg) }) }); False -> (case _58 of { True -> (case _64 of { True -> (case _ccε'4 of { (:) ρ'134 ρ'135 -> (case _61 of { (:) ρ'136 ρ'137 -> (let (:) _ arg = _57 in arg); [] -> (let (:) _ arg = ψ'4 in arg) }); [] -> (case π'26 of { (:) ρ'138 ρ'139 -> (let (:) _ arg = _57 in arg); [] -> (let (:) _ arg = ψ'4 in arg) }) }); False -> (case π'32 of { (:) ρ'140 ρ'141 -> (let (:) _ arg = _57 in arg); [] -> (let (:) _ arg = ψ'4 in arg) }) }); False -> (case π'35 of { (:) ρ'142 ρ'143 -> (let (:) _ arg = _57 in arg); [] -> (let (:) _ arg = ψ'4 in arg) }) }) }); [] -> (let (:) _ arg = ψ'4 in arg) }); [] -> (let (:) _ arg = π'29 in arg) }); False -> (let (:) _ arg = π'33 in arg) }); False -> (let (:) _ arg = π'36 in arg) }) }); False -> (let (:) _ arg = _cfε'3 in arg) }
  ret'10 = (call'18 False undefined undefined undefined _cfε'5 undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) π'37 π'38 π'39)
  in (,,,,,,,,) t'15 _65 π'27 ret'11 _60 _59 _62 ((π'39 : (case flag_ds'3 of { True -> (case _62 of { True -> (case _60 of { True -> (let (:) arg _ = π'30 in arg); False -> (let (:) arg _ = π'34 in arg) }); False -> (case _65 of { True -> (case _59 of { True -> (case t'15 of { (:) ρ'106 ρ'107 -> (case _cfε'4 of { (:) ρ'108 ρ'109 -> (case _63 of { True -> (case _56 of { True -> (case π'28 of { (:) ρ'110 ρ'111 -> (let (:) arg _ = _57 in arg); [] -> (let (:) arg _ = ψ'4 in arg) }); False -> (case π'31 of { (:) ρ'112 ρ'113 -> (let (:) arg _ = _57 in arg); [] -> (let (:) arg _ = ψ'4 in arg) }) }); False -> (case _58 of { True -> (case _64 of { True -> (case _ccε'4 of { (:) ρ'114 ρ'115 -> (case _61 of { (:) ρ'116 ρ'117 -> (let (:) arg _ = _57 in arg); [] -> (let (:) arg _ = ψ'4 in arg) }); [] -> (case π'26 of { (:) ρ'118 ρ'119 -> (let (:) arg _ = _57 in arg); [] -> (let (:) arg _ = ψ'4 in arg) }) }); False -> (case π'32 of { (:) ρ'120 ρ'121 -> (let (:) arg _ = _57 in arg); [] -> (let (:) arg _ = ψ'4 in arg) }) }); False -> (case π'35 of { (:) ρ'122 ρ'123 -> (let (:) arg _ = _57 in arg); [] -> (let (:) arg _ = ψ'4 in arg) }) }) }); [] -> (let (:) arg _ = ψ'4 in arg) }); [] -> (let (:) arg _ = π'29 in arg) }); False -> (let (:) arg _ = π'33 in arg) }); False -> (let (:) arg _ = π'36 in arg) }) }); False -> (let (:) arg _ = _cfε'3 in arg) })) : (case _cfε'5 of { (:) ρ'124 ρ'125 -> sel8 ret'10; [] -> sel4 ret'10 })) ret'11

call'16 _66 _67 = (call'6 (GHC.Enum.enumFromTo (1::Int) (_67 - Data.Foldable.length _66))) ++ _66

lam'5 _68 = \ds'6 -> ds'6 /= _68

alphabeticRule = lam'10

call'17 flag_ds'4'2 π'40 _69 π'41 _70 π'42 _71 π'43 π'44 _cfε'6 = 
  let ret'9 = (call'17 flag_ds'4'2 (let (:) _ arg = π'40 in arg) _69 π'41 _70 π'42 _71 π'43 π'44 _cfε'6) in
  (,) (call'8 flag_ds'4'2 _cfε'6 π'44 _69 π'43 _71 _70 π'41) (((call'16 (GHC.Show.show π'42) _71) ++ (let (:) arg _ = π'40 in arg)) : (case (let (:) _ arg = π'40 in arg) of { (:) ρ'144 ρ'145 -> sel2 ret'9; [] -> sel1 ret'9 }))

lam'9 = \regex -> Data.Foldable.foldl' lam (0::Int) (Data.Foldable.concat (call'23 regex))

call π'45 = let
  _72 = GHC.List.span (lam'6 (C# '-'#)) π'45
  _76 = GHC.List.span (lam'5 (C# '>'#)) (case _72 of { (,) ρ'150 ρ'151 -> (let (:) _ arg = ρ'151 in arg) })
  ret'3 = (call'13 (case _76 of { (,) ρ'148 ρ'149 -> ρ'148 }))
  ret'2 = (call'12 (case _72 of { (,) ρ'146 ρ'147 -> ρ'146 }))
  _74 = GHC.Enum.enumFromTo ret'2 ret'3
  _73 = ret'2 < ret'3
  _75 = GHC.Enum.enumFromThenTo ret'2 (ret'2 - (1::Int)) ret'3
  ret'15 = (call'11 _75 _73 _74 (max (Data.Foldable.length (GHC.Show.show ret'2)) (Data.Foldable.length (GHC.Show.show ret'3))) _76 (let (:) _ arg = (let (,) _ arg = _76 in arg) in arg))
  in (,,,,) _73 _74 _75 (sel2 ret'15) (sel1 ret'15)

call'19 _77 _78 _79 t'16 π'46 _80 t'17 _81 _82 _83 _84 _85 _86 _ccε'5 _87 _88 t'18 t'19 _89 _90 _91 π'47 = let
  ret'16 = (call'24 True (let (:) _ arg = undefined in arg) _77 _87 _78 (let (:) _ arg = t'18 in arg) _80 _81 _84 (let (:) _ arg = t'19 in arg) (let (:) _ arg = t'18 in arg) _89 _85 _88 π'46 (let (:) _ arg = t'17 in arg) _90 π'47)
  _94 = sel5 ret'16
  _93 = case sel4 ret'16 of { (:) ρ'174 ρ'175 -> (case sel7 ret'16 of { True -> (case sel6 ret'16 of { True -> (case (let (:) _ arg = t'18 in arg) of { (:) ρ'176 ρ'177 -> _94; [] -> [] }); False -> (case (let (:) _ arg = t'18 in arg) of { (:) ρ'178 ρ'179 -> _94; [] -> [] }) }); False -> (case sel1 ret'16 of { True -> (case sel3 ret'16 of { True -> (case (let (:) _ arg = t'17 in arg) of { (:) ρ'180 ρ'181 -> _94; [] -> [] }); False -> (case (let (:) _ arg = t'19 in arg) of { (:) ρ'182 ρ'183 -> _94; [] -> [] }) }); False -> (case sel2 ret'16 of { (:) ρ'184 ρ'185 -> _94; [] -> [] }) }) }); [] -> [] }
  _92 = π'47 : (case π'46 of { (:) ρ'172 ρ'173 -> (case _84 of { True -> (case _90 of { True -> (let (:) arg _ = t'18 in arg); False -> (let (:) arg _ = t'18 in arg) }); False -> (case _88 of { True -> (case _81 of { True -> (let (:) arg _ = t'17 in arg); False -> (let (:) arg _ = t'19 in arg) }); False -> _82 }) }); [] -> [] })
  _95 = _92 : _93
  _ccε'6 = case π'46 of { (:) ρ'152 ρ'153 -> (case _84 of { True -> (case _90 of { True -> (case _91 of { (:) ρ'154 ρ'155 -> (case t'18 of { (:) ρ'156 ρ'157 -> _95; [] -> [] }); [] -> [] }); False -> (case _86 of { (:) ρ'158 ρ'159 -> (case t'18 of { (:) ρ'160 ρ'161 -> _95; [] -> [] }); [] -> [] }) }); False -> (case _88 of { True -> (case _81 of { True -> (case _79 of { (:) ρ'162 ρ'163 -> (case t'17 of { (:) ρ'164 ρ'165 -> _95; [] -> [] }); [] -> [] }); False -> (case _83 of { (:) ρ'166 ρ'167 -> (case t'19 of { (:) ρ'168 ρ'169 -> _95; [] -> [] }); [] -> [] }) }); False -> (case t'16 of { (:) ρ'170 ρ'171 -> _95; [] -> [] }) }) }); [] -> _95 }
  in (,,,,,) _ccε'6 _92 _93 _94 _ccε'5 _ccε'6

call'22 π'48 π'49 t'20 ψ'5 π'50 _cfε'7 _96 _97 t'21 _98 _99 ψ'6 π'51 _100 _101 _102 _103 _104 _105 _106 _107 t'22 _108 t'23 π'52 _109 t'24 π'53 _110 _ccε'7 t'25 _ccε'8 _111 π'54 π'55 π'56 = let
  ret'17 = (call'25 True (let (:) _ arg = t'25 in arg) t'24 _100 π'49 (let (:) _ arg = t'23 in arg) (let (:) _ arg = undefined in arg) _cfε'7 π'48 (let (:) _ arg = t'23 in arg) (let (:) _ arg = ψ'6 in arg) _111 π'52 _109 _107 _102 _101 _99 _97 π'51 _105 π'50 _96 π'53 _106 ψ'5 (let (:) _ arg = t'20 in arg) _ccε'8 π'54 π'55 π'56)
  _116 = sel1 ret'17
  _113 = sel8 ret'17
  _114 = case sel9 ret'17 of { (:) ρ'210 ρ'211 -> (case sel2 ret'17 of { True -> (case sel7 ret'17 of { True -> (case (let (:) _ arg = t'23 in arg) of { (:) ρ'212 ρ'213 -> _113; [] -> _116 }); False -> (case (let (:) _ arg = t'23 in arg) of { (:) ρ'214 ρ'215 -> _113; [] -> _116 }) }); False -> (case sel5 ret'17 of { True -> (case sel10 ret'17 of { True -> (case (let (:) _ arg = t'25 in arg) of { (:) ρ'216 ρ'217 -> _113; [] -> _116 }); False -> (case sel4 ret'17 of { (:) ρ'218 ρ'219 -> (case sel3 ret'17 of { (:) ρ'220 ρ'221 -> _113; [] -> _116 }); [] -> (case (let (:) _ arg = ψ'6 in arg) of { (:) ρ'222 ρ'223 -> _113; [] -> _116 }) }) }); False -> (case (let (:) _ arg = t'20 in arg) of { (:) ρ'224 ρ'225 -> _113; [] -> _116 }) }) }); [] -> _116 }
  ret'18 = (call'21 π'56 π'54)
  _112 = π'55 : (case π'48 of { (:) ρ'206 ρ'207 -> (case _111 of { True -> (case _101 of { True -> (let (:) arg _ = t'23 in arg); False -> (let (:) arg _ = t'23 in arg) }); False -> (case _107 of { True -> (case _106 of { True -> (let (:) arg _ = t'25 in arg); False -> (case t'24 of { (:) ρ'208 ρ'209 -> _103; [] -> (let (:) arg _ = ψ'6 in arg) }) }); False -> (let (:) arg _ = t'20 in arg) }) }); [] -> [] })
  _115 = _112 : _114
  _ccε'9 = case π'48 of { (:) ρ'186 ρ'187 -> (case _111 of { True -> (case _101 of { True -> (case _110 of { (:) ρ'188 ρ'189 -> (case t'23 of { (:) ρ'190 ρ'191 -> _115; [] -> ret'18 }); [] -> ret'18 }); False -> (case _104 of { (:) ρ'192 ρ'193 -> (case t'23 of { (:) ρ'194 ρ'195 -> _115; [] -> ret'18 }); [] -> ret'18 }) }); False -> (case _107 of { True -> (case _106 of { True -> (case _98 of { (:) ρ'196 ρ'197 -> (case t'25 of { (:) ρ'198 ρ'199 -> _115; [] -> ret'18 }); [] -> ret'18 }); False -> (case _108 of { (:) ρ'200 ρ'201 -> (case t'22 of { (:) ρ'202 ρ'203 -> _115; [] -> ret'18 }); [] -> ret'18 }) }); False -> (case t'21 of { (:) ρ'204 ρ'205 -> _115; [] -> ret'18 }) }) }); [] -> _115 }
  in (,,,,,,,,,,,,) _ccε'7 (let (:) _ arg = t'25 in arg) _ccε'9 _112 (let (:) _ arg = t'23 in arg) (let (:) _ arg = t'20 in arg) (sel6 ret'17) _113 _114 ret'18 _ccε'9 (let (:) _ arg = ψ'6 in arg) (let (:) _ arg = t'23 in arg)

lam'10 = \ds'7 -> case ((let (:) arg _ = (let (:) _ arg = ds'7 in arg) in arg) == C# '-'#) && (((let (:) arg _ = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg) in arg) == C# ']'#) && ((let (:) arg _ = ds'7 in arg) <= (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg))) of { True -> sel12 (call'20 (GHC.Enum.enumFromTo (let (:) arg _ = ds'7 in arg) (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg)) (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg) in arg)); False -> sel2 (call'26 (GHC.List.reverse (GHC.Enum.enumFromTo (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg) (let (:) arg _ = ds'7 in arg))) (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg) in arg)) }

call'2 flag_ds π'57 _117 ds'8 ds'9 = let
  π'58 = case flag_ds of { True -> (case _117 of { (,) ρ'226 ρ'227 -> (let (:) _ arg = π'57 in arg) }); False -> (let (:) _ arg = ds'9 in arg) }
  ret'12 = (call'14 π'58)
  _120 = sel19 ret'12
  _121 = sel4 ret'12
  _118 = sel3 ret'12
  _119 = sel17 ret'12
  ret'19 = (call'19 (sel11 ret'12) (sel13 ret'12) (sel2 ret'12) (sel6 ret'12) π'58 (sel10 ret'12) (sel1 ret'12) _121 (sel12 ret'12) (sel18 ret'12) _120 (sel9 ret'12) (sel16 ret'12) (sel8 ret'12) (sel7 ret'12) _119 (sel14 ret'12) (sel15 ret'12) (sel5 ret'12) _118 (sel20 ret'12) (case flag_ds of { True -> (case _117 of { (,) ρ'228 ρ'229 -> (let (:) arg _ = π'57 in arg) }); False -> (let (:) arg _ = ds'8 in arg) }))
  in (,,,,,,,,,) (sel3 ret'19) (sel4 ret'19) _118 (sel5 ret'19) (sel1 ret'19) _119 (sel6 ret'19) _120 _121 (sel2 ret'19)

call'26 _122 π'59 = let
  ret'14 = (call'15 π'59)
  _127 = sel14 ret'14
  _ccε'10 = sel18 ret'14
  _125 = sel30 ret'14
  _126 = sel25 ret'14
  _124 = sel33 ret'14
  _123 = sel4 ret'14
  ret'20 = (call'22 π'59 (sel2 ret'14) (sel15 ret'14) (sel11 ret'14) (sel28 ret'14) (sel5 ret'14) (sel22 ret'14) _124 (sel19 ret'14) (sel3 ret'14) (sel6 ret'14) (sel17 ret'14) (sel32 ret'14) (sel12 ret'14) _127 (sel26 ret'14) (sel1 ret'14) (sel10 ret'14) (sel16 ret'14) _123 _126 (sel23 ret'14) (sel29 ret'14) (sel21 ret'14) (sel20 ret'14) (sel27 ret'14) (sel9 ret'14) (sel8 ret'14) (sel31 ret'14) _ccε'10 (sel13 ret'14) (sel24 ret'14) _125 π'59 (let (:) arg _ = _122 in arg) (let (:) _ arg = _122 in arg))
  in (,,,,,,,,,,,,,,,,,,,) (sel10 ret'20) (case _122 of { (:) ρ'230 ρ'231 -> sel11 ret'20; [] -> [] }) (sel7 ret'14) (sel4 ret'20) _123 (sel3 ret'20) (sel12 ret'20) (sel7 ret'20) (sel6 ret'20) _124 (sel13 ret'20) (sel5 ret'20) _ccε'10 _125 _126 (sel9 ret'20) (sel1 ret'20) _127 (sel8 ret'20) (sel2 ret'20)

numericRule = lam'4

call'7 π'60 = case π'60 of { (:) ρ'232 ρ'233 -> C# '0'# : (call'7 ρ'233); [] -> [] }

call'25 flag_ds'2 π'61 t'26 _128 π'62 π'63 _cfε'8 _cfε'9 π'64 π'65 π'66 _129 π'67 _130 _131 _132 _133 _134 _135 π'68 _136 π'69 _137 π'70 _138 ψ'7 π'71 _ccε'11 π'72 π'73 π'74 = let
  ret'18 = (call'21 π'74 π'72)
  _cfε'10 = case flag_ds'2 of { True -> (case _129 of { True -> (case _133 of { True -> (let (:) _ arg = π'65 in arg); False -> (let (:) _ arg = π'63 in arg) }); False -> (case _131 of { True -> (case _138 of { True -> (let (:) _ arg = π'61 in arg); False -> (case t'26 of { (:) ρ'254 ρ'255 -> (case _cfε'9 of { (:) ρ'256 ρ'257 -> (case _132 of { True -> (case _137 of { True -> (case π'68 of { (:) ρ'258 ρ'259 -> (let (:) _ arg = _134 in arg); [] -> (let (:) _ arg = ψ'7 in arg) }); False -> (case π'62 of { (:) ρ'260 ρ'261 -> (let (:) _ arg = _134 in arg); [] -> (let (:) _ arg = ψ'7 in arg) }) }); False -> (case _130 of { True -> (case _128 of { True -> (case π'67 of { (:) ρ'262 ρ'263 -> (let (:) _ arg = _134 in arg); [] -> (let (:) _ arg = ψ'7 in arg) }); False -> (case _ccε'11 of { (:) ρ'264 ρ'265 -> (case _136 of { (:) ρ'266 ρ'267 -> (let (:) _ arg = _134 in arg); [] -> (let (:) _ arg = ψ'7 in arg) }); [] -> (case π'70 of { (:) ρ'268 ρ'269 -> (let (:) _ arg = _134 in arg); [] -> (let (:) _ arg = ψ'7 in arg) }) }) }); False -> (case π'69 of { (:) ρ'270 ρ'271 -> (let (:) _ arg = _134 in arg); [] -> (let (:) _ arg = ψ'7 in arg) }) }) }); [] -> (let (:) _ arg = ψ'7 in arg) }); [] -> (let (:) _ arg = π'66 in arg) }) }); False -> (let (:) _ arg = π'71 in arg) }) }); False -> (let (:) _ arg = _cfε'8 in arg) }
  ret'17 = (call'25 False (let (:) _ arg = undefined in arg) undefined undefined undefined (let (:) _ arg = undefined in arg) _cfε'10 undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) undefined π'72 π'73 π'74)
  in (,,,,,,,,,) ret'18 _129 _135 t'26 _131 ret'18 _133 ((π'73 : (case flag_ds'2 of { True -> (case _129 of { True -> (case _133 of { True -> (let (:) arg _ = π'65 in arg); False -> (let (:) arg _ = π'63 in arg) }); False -> (case _131 of { True -> (case _138 of { True -> (let (:) arg _ = π'61 in arg); False -> (case t'26 of { (:) ρ'234 ρ'235 -> (case _cfε'9 of { (:) ρ'236 ρ'237 -> (case _132 of { True -> (case _137 of { True -> (case π'68 of { (:) ρ'238 ρ'239 -> (let (:) arg _ = _134 in arg); [] -> (let (:) arg _ = ψ'7 in arg) }); False -> (case π'62 of { (:) ρ'240 ρ'241 -> (let (:) arg _ = _134 in arg); [] -> (let (:) arg _ = ψ'7 in arg) }) }); False -> (case _130 of { True -> (case _128 of { True -> (case π'67 of { (:) ρ'242 ρ'243 -> (let (:) arg _ = _134 in arg); [] -> (let (:) arg _ = ψ'7 in arg) }); False -> (case _ccε'11 of { (:) ρ'244 ρ'245 -> (case _136 of { (:) ρ'246 ρ'247 -> (let (:) arg _ = _134 in arg); [] -> (let (:) arg _ = ψ'7 in arg) }); [] -> (case π'70 of { (:) ρ'248 ρ'249 -> (let (:) arg _ = _134 in arg); [] -> (let (:) arg _ = ψ'7 in arg) }) }) }); False -> (case π'69 of { (:) ρ'250 ρ'251 -> (let (:) arg _ = _134 in arg); [] -> (let (:) arg _ = ψ'7 in arg) }) }) }); [] -> (let (:) arg _ = ψ'7 in arg) }); [] -> (let (:) arg _ = π'66 in arg) }) }); False -> (let (:) arg _ = π'71 in arg) }) }); False -> (let (:) arg _ = _cfε'8 in arg) })) : (case _cfε'10 of { (:) ρ'252 ρ'253 -> sel8 ret'17; [] -> sel1 ret'17 })) π'64 _138

call'12 t'27 = Data.Foldable.foldl lam'8 (0::Int) t'27

call' flag_ds' _cfε'11 π'75 _139 = let
  π'76 = case flag_ds' of { True -> (case _139 of { (,) ρ'272 ρ'273 -> (let (:) arg _ = π'75 in arg) }); False -> (let (:) arg _ = _cfε'11 in arg) }
  π'77 = case flag_ds' of { True -> (case _139 of { (,) ρ'280 ρ'281 -> (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = π'75 in arg) in arg) in arg) }); False -> (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = _cfε'11 in arg) in arg) in arg) }
  _142 = GHC.List.reverse (GHC.Enum.enumFromTo π'77 π'76)
  π'78 = case flag_ds' of { True -> (case _139 of { (,) ρ'278 ρ'279 -> (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = π'75 in arg) in arg) in arg) in arg) }); False -> (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = _cfε'11 in arg) in arg) in arg) in arg) }
  ret'21 = (call'26 _142 π'78)
  _140 = GHC.Enum.enumFromTo π'76 π'77
  ret'22 = (call'20 _140 π'78)
  _141 = ((case flag_ds' of { True -> (case _139 of { (,) ρ'274 ρ'275 -> (let (:) arg _ = (let (:) _ arg = π'75 in arg) in arg) }); False -> (let (:) arg _ = (let (:) _ arg = _cfε'11 in arg) in arg) }) == C# '-'#) && (((case flag_ds' of { True -> (case _139 of { (,) ρ'276 ρ'277 -> (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = π'75 in arg) in arg) in arg) in arg) }); False -> (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = _cfε'11 in arg) in arg) in arg) in arg) }) == C# ']'#) && (π'76 <= π'77))
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) (sel14 ret'21) (sel18 ret'22) (sel1 ret'22) (sel6 ret'22) (sel7 ret'22) (sel3 ret'22) (sel15 ret'22) (sel7 ret'21) (sel9 ret'22) (sel5 ret'22) (sel15 ret'21) (sel3 ret'21) (sel1 ret'21) (sel17 ret'21) (sel9 ret'21) (sel10 ret'21) (sel13 ret'22) _140 (sel2 ret'22) (sel11 ret'21) (sel19 ret'21) (sel20 ret'21) (sel4 ret'22) (sel13 ret'21) (sel12 ret'21) (sel11 ret'22) (sel8 ret'21) (case _141 of { True -> sel12 ret'22; False -> sel2 ret'21 }) (sel16 ret'21) (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = _cfε'11 in arg) in arg) in arg) in arg) (sel17 ret'22) (sel20 ret'22) _142 (sel4 ret'21) (sel18 ret'21) (sel5 ret'21) (sel16 ret'22) (sel19 ret'22) (sel14 ret'22) (sel6 ret'21) (sel8 ret'22) (sel10 ret'22) _141

call'24 flag_ds' _cfε'12 _143 _144 _145 π'79 _146 _147 _148 π'80 π'81 _149 _150 _151 π'82 π'83 _152 π'84 = 
  let _cfε'13 = case flag_ds' of { True -> (case _148 of { True -> (case _152 of { True -> (let (:) _ arg = π'81 in arg); False -> (let (:) _ arg = π'79 in arg) }); False -> (case _151 of { True -> (case _147 of { True -> (let (:) _ arg = π'83 in arg); False -> (let (:) _ arg = π'80 in arg) }); False -> (case _146 of { True -> (case _143 of { True -> (let (:) _ arg = _149 in arg); False -> (let (:) _ arg = _149 in arg) }); False -> (case _145 of { True -> (case _144 of { True -> (let (:) _ arg = _149 in arg); False -> (let (:) _ arg = _149 in arg) }); False -> (let (:) _ arg = _149 in arg) }) }) }) }); False -> (let (:) _ arg = _cfε'12 in arg) } in
  (,,,,,,) _151 _150 _147 π'82 ((π'84 : (case flag_ds' of { True -> (case _148 of { True -> (case _152 of { True -> (let (:) arg _ = π'81 in arg); False -> (let (:) arg _ = π'79 in arg) }); False -> (case _151 of { True -> (case _147 of { True -> (let (:) arg _ = π'83 in arg); False -> (let (:) arg _ = π'80 in arg) }); False -> (case _146 of { True -> (case _143 of { True -> (let (:) arg _ = _149 in arg); False -> (let (:) arg _ = _149 in arg) }); False -> (case _145 of { True -> (case _144 of { True -> (let (:) arg _ = _149 in arg); False -> (let (:) arg _ = _149 in arg) }); False -> (let (:) arg _ = _149 in arg) }) }) }) }); False -> (let (:) arg _ = _cfε'12 in arg) })) : (case _cfε'13 of { (:) ρ'282 ρ'283 -> sel5 (call'24 False _cfε'13 undefined undefined undefined (let (:) _ arg = undefined in arg) undefined undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined (let (:) _ arg = undefined in arg) undefined π'84); [] -> [] })) _152 _148

lam'7 acc' = \c' -> ord c' + (acc' * (31::Int))

call'4 π'85 = let
  ret'5 = (call' False (let (:) _ arg = π'85 in arg) (let (:) _ arg = undefined in arg) undefined)
  ret'4 = (call'2 False undefined undefined π'85 π'85)
  ret'6 = (call (let (:) _ arg = π'85 in arg))
  _153 = (let (:) arg _ = π'85 in arg) == GHC.List.head (GHC.CString.unpackCString# "<"#)
  _154 = (let (:) arg _ = π'85 in arg) == GHC.List.head (GHC.CString.unpackCString# "["#)
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) (sel32 ret'5) (sel18 ret'5) (sel43 ret'5) (sel30 ret'5) (sel17 ret'5) (sel26 ret'5) (sel10 ret'5) (sel14 ret'5) (sel6 ret'5) (sel41 ret'5) (sel3 ret'6) (sel7 ret'5) (sel37 ret'5) (sel19 ret'5) (sel39 ret'5) (sel1 ret'6) (sel38 ret'5) (sel5 ret'4) (sel42 ret'5) (sel13 ret'5) (sel23 ret'5) (case π'85 of { (:) ρ'284 ρ'285 -> (case _153 of { True -> sel4 ret'6; False -> (case _154 of { True -> sel28 ret'5; False -> sel7 ret'4 }) }); [] -> [] : [] }) (sel4 ret'5) (sel31 ret'5) (sel9 ret'5) (sel5 ret'5) (sel5 ret'6) (sel40 ret'5) _154 (sel3 ret'5) (sel2 ret'5) (sel33 ret'5) _153 (sel2 ret'6)

lam'6 _155 = \ds'10 -> ds'10 /= _155

expand = lam'2

call'9 π'86 _156 = let
  ret'5 = (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'86 in arg) _156)
  ret'6 = (call (case _156 of { (,) ρ'288 ρ'289 -> (let (:) _ arg = π'86 in arg) }))
  π'87 = case _156 of { (,) ρ'286 ρ'287 -> (let (:) arg _ = π'86 in arg) }
  in (,,,,,,,,,,,,,,) (sel32 ret'5) (sel39 ret'5) (sel1 ret'6) (sel38 ret'5) (sel43 ret'5) (sel18 ret'5) (sel5 (call'2 True π'86 _156 undefined undefined)) (sel5 ret'6) (sel40 ret'5) (sel3 ret'6) (π'87 == GHC.List.head (GHC.CString.unpackCString# "["#)) (sel3 ret'5) (sel33 ret'5) (π'87 == GHC.List.head (GHC.CString.unpackCString# "<"#)) (sel2 ret'6)

call'23 regex' = case regex' of { (:) ρ'290 ρ'291 -> (case ρ'290 == GHC.List.head (GHC.CString.unpackCString# "<"#) of { True -> sel4 (call ρ'291); False -> (case ρ'290 == GHC.List.head (GHC.CString.unpackCString# "["#) of { True -> sel28 (call' False ρ'291 (let (:) _ arg = undefined in arg) undefined); False -> sel7 (call'2 False undefined undefined regex' regex') }) }); [] -> [] : [] }
