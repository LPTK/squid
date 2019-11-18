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

call'3 π π' = case π of { (:) ρ'2 ρ'3 -> sel6 (call'4 π' (sel6 (call'5 π')) (sel26 (call'5 π')) (sel24 (call'5 π')) (sel7 (call'5 π')) (sel23 (call'5 π')) (sel20 (call'5 π')) (sel16 (call'5 π')) (sel27 (call'5 π')) (sel11 (call'5 π')) (sel9 (call'5 π')) (sel17 (call'5 π')) (sel10 (call'5 π')) (sel14 (call'5 π')) (sel19 (call'5 π')) (sel33 (call'5 π')) (sel32 (call'5 π')) (sel2 (call'5 π')) (sel4 (call'5 π')) (sel12 (call'5 π')) (sel3 (call'5 π')) (sel28 (call'5 π')) (sel30 (call'5 π')) (sel8 (call'5 π')) (sel18 (call'5 π')) (sel34 (call'5 π')) (sel21 (call'5 π')) (sel25 (call'5 π')) (sel15 (call'5 π')) (sel5 (call'5 π')) (sel22 (call'5 π')) (sel29 (call'5 π')) (sel1 (call'5 π')) (sel13 (call'5 π')) π' ρ'3 ρ'2); [] -> [] }

constantRule = lam'3

call'6 _0 = case _0 of { (:) ρ'4 ρ'5 -> C# '0'# : (call'7 ρ'5); [] -> [] }

call'8 flag_ds'4 _cfε π'2 _1 π'3 _2 _3 π'4 = 
  let t = sel2 (call'9 False (sel1 (call'10 π'4 _3)) (sel2 (call'10 π'4 _3)) (sel10 (call'10 π'4 _3)) (sel14 (call'10 π'4 _3)) _3 (sel11 (call'10 π'4 _3)) (sel8 (call'10 π'4 _3)) (sel9 (call'10 π'4 _3)) (sel12 (call'10 π'4 _3)) (sel13 (call'10 π'4 _3)) (sel7 (call'10 π'4 _3)) (sel6 (call'10 π'4 _3)) (sel5 (call'10 π'4 _3)) (sel4 (call'10 π'4 _3)) (sel15 (call'10 π'4 _3)) π'4 (sel3 (call'10 π'4 _3)) undefined π'4 _3 (case flag_ds'4 of { True -> (case _1 of { True -> (let (:) arg _ = π'3 in arg); False -> (let (:) arg _ = π'2 in arg) }); False -> (let (:) arg _ = _cfε in arg) }) _2 (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) (case flag_ds'4 of { True -> (case _1 of { True -> (let (:) _ arg = π'3 in arg); False -> (let (:) _ arg = π'2 in arg) }); False -> (let (:) _ arg = _cfε in arg) })) in
  case flag_ds'4 of { True -> (case _1 of { True -> (case π'3 of { (:) ρ'6 ρ'7 -> t; [] -> [] }); False -> (case π'2 of { (:) ρ'8 ρ'9 -> t; [] -> [] }) }); False -> (case _cfε of { (:) ρ'10 ρ'11 -> t; [] -> [] }) }

lam'4 = \x -> let
  _7 = GHC.List.span (lam'6 (C# '-'#)) x
  _6 = GHC.List.span (lam'5 (C# '>'#)) (case _7 of { (,) ρ'16 ρ'17 -> (let (:) _ arg = ρ'17 in arg) })
  _5 = (call'13 (case _6 of { (,) ρ'14 ρ'15 -> ρ'14 }))
  _4 = (call'12 (case _7 of { (,) ρ'12 ρ'13 -> ρ'12 }))
  in sel2 (call'11 (GHC.Enum.enumFromThenTo _4 (_4 - (1::Int)) _5) (_4 < _5) (GHC.Enum.enumFromTo _4 _5) (max (Data.Foldable.length (GHC.Show.show _4)) (Data.Foldable.length (GHC.Show.show _5))) _6 (let (:) _ arg = (let (,) _ arg = _6 in arg) in arg))

call'14 π'5 = let
  _8 = (let (:) arg _ = π'5 in arg) == GHC.List.head (GHC.CString.unpackCString# "["#)
  _9 = (let (:) arg _ = π'5 in arg) == GHC.List.head (GHC.CString.unpackCString# "<"#)
  in (,,,,,,,,,,,,,,,,,,,) (sel39 (call' False (let (:) _ arg = π'5 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel18 (call' False (let (:) _ arg = π'5 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel1 (call (let (:) _ arg = π'5 in arg))) (sel43 (call' False (let (:) _ arg = π'5 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel2 (call'2 False undefined undefined π'5 π'5)) (sel5 (call'2 False undefined undefined π'5 π'5)) (sel9 (call'2 False undefined undefined π'5 π'5)) (case π'5 of { (:) ρ'18 ρ'19 -> (case _9 of { True -> sel4 (call ρ'19); False -> (case _8 of { True -> sel28 (call' False ρ'19 (let (:) _ arg = undefined in arg) undefined); False -> sel7 (call'2 False undefined undefined π'5 π'5) }) }); [] -> [] : [] }) (sel1 (call'2 False undefined undefined π'5 π'5)) (sel8 (call'2 False undefined undefined π'5 π'5)) (sel3 (call'2 False undefined undefined π'5 π'5)) (sel10 (call'2 False undefined undefined π'5 π'5)) (sel6 (call'2 False undefined undefined π'5 π'5)) (sel5 (call (let (:) _ arg = π'5 in arg))) (sel40 (call' False (let (:) _ arg = π'5 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel3 (call (let (:) _ arg = π'5 in arg))) _8 (sel33 (call' False (let (:) _ arg = π'5 in arg) (let (:) _ arg = undefined in arg) undefined)) _9 (sel2 (call (let (:) _ arg = π'5 in arg)))

lam = \acc -> (lam'7 acc)

call'15 π'6 = let
  _10 = (let (:) arg _ = π'6 in arg) == GHC.List.head (GHC.CString.unpackCString# "<"#)
  _11 = (let (:) arg _ = π'6 in arg) == GHC.List.head (GHC.CString.unpackCString# "["#)
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) (sel34 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel25 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel18 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel43 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel30 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel21 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel24 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel8 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel14 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel3 (call (let (:) _ arg = π'6 in arg))) (sel27 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel36 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel39 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel1 (call (let (:) _ arg = π'6 in arg))) (sel5 (call'2 False undefined undefined π'6 π'6)) (sel16 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel13 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (case π'6 of { (:) ρ'20 ρ'21 -> (case _10 of { True -> sel4 (call ρ'21); False -> (case _11 of { True -> sel28 (call' False ρ'21 (let (:) _ arg = undefined in arg) undefined); False -> sel7 (call'2 False undefined undefined π'6 π'6) }) }); [] -> [] : [] }) (sel4 (call'2 False undefined undefined π'6 π'6)) (sel22 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel5 (call (let (:) _ arg = π'6 in arg))) (sel35 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel40 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel12 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) _11 (sel1 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel11 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel15 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel33 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) _10 (sel2 (call (let (:) _ arg = π'6 in arg))) (sel20 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel29 (call' False (let (:) _ arg = π'6 in arg) (let (:) _ arg = undefined in arg) undefined))

call'11 _12 _13 _14 _15 _16 π'7 = let
  _17 = sel15 (call'10 π'7 _16)
  t' = sel2 (call'10 π'7 _16)
  _22 = sel13 (call'10 π'7 _16)
  _20 = sel11 (call'10 π'7 _16)
  t'5 = sel4 (call'10 π'7 _16)
  t'2 = sel8 (call'10 π'7 _16)
  _25 = sel3 (call'10 π'7 _16)
  _24 = sel5 (call'10 π'7 _16)
  t'3 = sel9 (call'10 π'7 _16)
  _21 = sel12 (call'10 π'7 _16)
  _18 = sel10 (call'10 π'7 _16)
  _23 = sel6 (call'10 π'7 _16)
  _19 = sel14 (call'10 π'7 _16)
  t'4 = sel7 (call'10 π'7 _16)
  ψ = sel1 (call'10 π'7 _16)
  π'8 = case _13 of { True -> (let (:) arg _ = _14 in arg); False -> (let (:) arg _ = _12 in arg) }
  t'6 = sel2 (call'9 True ψ t' _18 _19 _16 _20 t'2 t'3 _21 _22 t'4 _23 _24 t'5 _17 π'7 _25 _13 π'7 _16 π'8 _15 (let (:) _ arg = _14 in arg) (let (:) _ arg = _12 in arg) (let (:) _ arg = undefined in arg))
  in (,) (sel1 (call'9 True ψ t' _18 _19 _16 _20 t'2 t'3 _21 _22 t'4 _23 _24 t'5 _17 π'7 _25 _13 π'7 _16 π'8 _15 (let (:) _ arg = _14 in arg) (let (:) _ arg = _12 in arg) (let (:) _ arg = undefined in arg))) (case _13 of { True -> (case _14 of { (:) ρ'22 ρ'23 -> t'6; [] -> [] }); False -> (case _12 of { (:) ρ'24 ρ'25 -> t'6; [] -> [] }) })

call'9 flag_ds'4' ψ' t'7 _26 _27 _28 _29 t'8 t'9 _30 _31 t'10 _32 _33 t'11 _34 π'9 _35 _36 π'10 _37 π'11 _38 π'12 π'13 _cfε' = let
  π'14 = case _28 of { (,) ρ'26 ρ'27 -> (case π'9 of { (:) ρ'28 ρ'29 -> (case _27 of { True -> (case _35 of { True -> (let (:) _ arg = t'8 in arg); False -> (let (:) _ arg = t'8 in arg) }); False -> (case _29 of { True -> (case _33 of { True -> (let (:) _ arg = t'7 in arg); False -> (let (:) _ arg = t'9 in arg) }); False -> (let (:) _ arg = t'10 in arg) }) }); [] -> [] }) }
  _39 = ((call'16 (GHC.Show.show π'11) _38) ++ (case _28 of { (,) ρ'52 ρ'53 -> (case π'9 of { (:) ρ'54 ρ'55 -> (case _27 of { True -> (case _35 of { True -> (let (:) arg _ = t'8 in arg); False -> (let (:) arg _ = t'8 in arg) }); False -> (case _29 of { True -> (case _33 of { True -> (case t'11 of { (:) ρ'56 ρ'57 -> _30; [] -> (let (:) arg _ = ψ' in arg) }); False -> (let (:) arg _ = t'9 in arg) }); False -> (let (:) arg _ = t'10 in arg) }) }); [] -> [] }) })) : (case π'14 of { (:) ρ'58 ρ'59 -> sel2 (call'17 flag_ds'4' π'14 _36 π'10 _37 π'11 _38 π'12 π'13 _cfε'); [] -> sel1 (call'17 flag_ds'4' π'14 _36 π'10 _37 π'11 _38 π'12 π'13 _cfε') })
  _ccε = (call'8 flag_ds'4' _cfε' π'13 _36 π'12 _38 _37 π'10)
  t'12 = case _28 of { (,) ρ'30 ρ'31 -> (case π'9 of { (:) ρ'32 ρ'33 -> (case _27 of { True -> (case _35 of { True -> (case _34 of { (:) ρ'34 ρ'35 -> (case t'8 of { (:) ρ'36 ρ'37 -> _39; [] -> _ccε }); [] -> _ccε }); False -> (case _26 of { (:) ρ'38 ρ'39 -> (case t'8 of { (:) ρ'40 ρ'41 -> _39; [] -> _ccε }); [] -> _ccε }) }); False -> (case _29 of { True -> (case _33 of { True -> (case _32 of { (:) ρ'42 ρ'43 -> (case t'7 of { (:) ρ'44 ρ'45 -> _39; [] -> _ccε }); [] -> _ccε }); False -> (case _31 of { (:) ρ'46 ρ'47 -> (case t'9 of { (:) ρ'48 ρ'49 -> _39; [] -> _ccε }); [] -> _ccε }) }); False -> (case t'10 of { (:) ρ'50 ρ'51 -> _39; [] -> _ccε }) }) }); [] -> _39 }) }
  in (,) t'12 t'12

call'4 π'15 _40 _41 ψ'2 π'16 π'17 ψ'3 _42 t'13 _43 _44 t'14 π'18 π'19 π'20 _45 _46 _47 _cfε'2 _ccε' _48 t'15 _49 t'16 t'17 _50 _51 _52 t'18 _53 _ccε'2 _54 ψ'4 _55 π'21 π'22 π'23 = let
  _56 = sel4 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23)
  _59 = sel8 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23)
  _58 = case sel3 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23) of { (:) ρ'86 ρ'87 -> (case sel7 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23) of { True -> (case sel5 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23) of { True -> (case (let (:) _ arg = t'13 in arg) of { (:) ρ'88 ρ'89 -> _59; [] -> _56 }); False -> (case (let (:) _ arg = t'13 in arg) of { (:) ρ'90 ρ'91 -> _59; [] -> _56 }) }); False -> (case sel2 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23) of { True -> (case sel6 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23) of { True -> (case sel1 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23) of { (:) ρ'92 ρ'93 -> (case _51 of { (:) ρ'94 ρ'95 -> _59; [] -> _56 }); [] -> (case (let (:) _ arg = ψ'4 in arg) of { (:) ρ'96 ρ'97 -> _59; [] -> _56 }) }); False -> (case (let (:) _ arg = t'15 in arg) of { (:) ρ'98 ρ'99 -> _59; [] -> _56 }) }); False -> (case (let (:) _ arg = t'17 in arg) of { (:) ρ'100 ρ'101 -> _59; [] -> _56 }) }) }); [] -> _56 }
  ψ'5 = (call'3 π'22 π'21)
  _57 = π'23 : (case π'15 of { (:) ρ'82 ρ'83 -> (case _45 of { True -> (case _42 of { True -> (let (:) arg _ = t'13 in arg); False -> (let (:) arg _ = t'13 in arg) }); False -> (case _54 of { True -> (case _48 of { True -> (case t'14 of { (:) ρ'84 ρ'85 -> _49; [] -> (let (:) arg _ = ψ'4 in arg) }); False -> (let (:) arg _ = t'15 in arg) }); False -> (let (:) arg _ = t'17 in arg) }) }); [] -> [] })
  _60 = _57 : _58
  _ccε'3 = case π'15 of { (:) ρ'60 ρ'61 -> (case _45 of { True -> (case _42 of { True -> (case _50 of { (:) ρ'62 ρ'63 -> (case t'13 of { (:) ρ'64 ρ'65 -> _60; [] -> ψ'5 }); [] -> ψ'5 }); False -> (case _43 of { (:) ρ'66 ρ'67 -> (case t'13 of { (:) ρ'68 ρ'69 -> _60; [] -> ψ'5 }); [] -> ψ'5 }) }); False -> (case _54 of { True -> (case _48 of { True -> (case _47 of { (:) ρ'70 ρ'71 -> (case t'18 of { (:) ρ'72 ρ'73 -> _60; [] -> ψ'5 }); [] -> ψ'5 }); False -> (case _46 of { (:) ρ'74 ρ'75 -> (case t'16 of { (:) ρ'76 ρ'77 -> _60; [] -> (case ψ'3 of { (:) ρ'78 ρ'79 -> _60; [] -> ψ'5 }) }); [] -> ψ'5 }) }); False -> (case t'17 of { (:) ρ'80 ρ'81 -> _60; [] -> ψ'5 }) }) }); [] -> _60 }
  in (,,,,,,,,,,,,) (let (:) _ arg = t'17 in arg) _ccε'2 (let (:) _ arg = t'15 in arg) (sel9 (call'18 True _52 _41 _53 (let (:) _ arg = undefined in arg) _48 π'19 π'15 _42 π'17 _55 (let (:) _ arg = ψ'4 in arg) (let (:) _ arg = t'13 in arg) t'14 _cfε'2 π'16 _45 _44 π'18 (let (:) _ arg = t'15 in arg) (let (:) _ arg = t'13 in arg) _40 _54 ψ'2 _ccε' π'20 (let (:) _ arg = t'17 in arg) π'21 π'22 π'23)) (let (:) _ arg = ψ'4 in arg) _ccε'3 (let (:) _ arg = t'13 in arg) _ccε'3 (let (:) _ arg = t'13 in arg) _57 ψ'5 _58 _59

lam'3 = \ds'5 -> sel6 (call'19 (sel11 (call'14 (let (:) _ arg = ds'5 in arg))) (sel13 (call'14 (let (:) _ arg = ds'5 in arg))) (sel2 (call'14 (let (:) _ arg = ds'5 in arg))) (sel6 (call'14 (let (:) _ arg = ds'5 in arg))) (let (:) _ arg = ds'5 in arg) (sel10 (call'14 (let (:) _ arg = ds'5 in arg))) (sel1 (call'14 (let (:) _ arg = ds'5 in arg))) (sel4 (call'14 (let (:) _ arg = ds'5 in arg))) (sel12 (call'14 (let (:) _ arg = ds'5 in arg))) (sel18 (call'14 (let (:) _ arg = ds'5 in arg))) (sel19 (call'14 (let (:) _ arg = ds'5 in arg))) (sel9 (call'14 (let (:) _ arg = ds'5 in arg))) (sel16 (call'14 (let (:) _ arg = ds'5 in arg))) (sel8 (call'14 (let (:) _ arg = ds'5 in arg))) (sel7 (call'14 (let (:) _ arg = ds'5 in arg))) (sel17 (call'14 (let (:) _ arg = ds'5 in arg))) (sel14 (call'14 (let (:) _ arg = ds'5 in arg))) (sel15 (call'14 (let (:) _ arg = ds'5 in arg))) (sel5 (call'14 (let (:) _ arg = ds'5 in arg))) (sel3 (call'14 (let (:) _ arg = ds'5 in arg))) (sel20 (call'14 (let (:) _ arg = ds'5 in arg))) (let (:) arg _ = ds'5 in arg))

lam'8 = \u' -> (lam' u')

call'13 t'19 = Data.Foldable.foldl lam'8 (0::Int) t'19

call'20 _61 π'24 = let
  _62 = sel13 (call'5 π'24)
  ψ'7 = sel20 (call'5 π'24)
  _71 = sel3 (call'5 π'24)
  _cfε'3 = sel4 (call'5 π'24)
  t'24 = sel18 (call'5 π'24)
  _ccε'4 = sel12 (call'5 π'24)
  _76 = sel5 (call'5 π'24)
  π'27 = sel10 (call'5 π'24)
  t'21 = sel17 (call'5 π'24)
  π'26 = sel23 (call'5 π'24)
  t'23 = sel8 (call'5 π'24)
  _63 = sel6 (call'5 π'24)
  _68 = sel33 (call'5 π'24)
  _ccε'5 = sel22 (call'5 π'24)
  _66 = sel11 (call'5 π'24)
  _75 = sel25 (call'5 π'24)
  π'28 = sel14 (call'5 π'24)
  _69 = sel32 (call'5 π'24)
  _72 = sel30 (call'5 π'24)
  π'25 = sel7 (call'5 π'24)
  _64 = sel26 (call'5 π'24)
  π'29 = sel19 (call'5 π'24)
  t'20 = sel27 (call'5 π'24)
  _74 = sel21 (call'5 π'24)
  _77 = sel29 (call'5 π'24)
  _70 = sel2 (call'5 π'24)
  ψ'8 = sel1 (call'5 π'24)
  ψ'6 = sel24 (call'5 π'24)
  t'22 = sel28 (call'5 π'24)
  _65 = sel16 (call'5 π'24)
  _73 = sel34 (call'5 π'24)
  t'25 = sel15 (call'5 π'24)
  _67 = sel9 (call'5 π'24)
  in (,,,,,,,,,,,,,,,,,,,) (sel10 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) (sel5 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) _68 (sel12 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) (sel7 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) (sel9 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) (sel13 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) (sel3 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) _65 (sel1 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) _71 (case _61 of { (:) ρ'102 ρ'103 -> sel6 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 ρ'103 ρ'102); [] -> [] }) _77 (sel8 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) (sel31 (call'5 π'24)) _74 (sel4 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) _ccε'5 (sel2 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg))) (sel11 (call'4 π'24 _63 _64 ψ'6 π'25 π'26 ψ'7 _65 t'20 _66 _67 t'21 π'27 π'28 π'29 _68 _69 _70 _cfε'3 _ccε'4 _71 t'22 _72 t'23 t'24 _73 _74 _75 t'25 _76 _ccε'5 _77 ψ'8 _62 π'24 (let (:) _ arg = _61 in arg) (let (:) arg _ = _61 in arg)))

call'21 π'30 π'31 = case π'30 of { (:) ρ'104 ρ'105 -> sel11 (call'22 π'31 (sel2 (call'15 π'31)) (sel15 (call'15 π'31)) (sel11 (call'15 π'31)) (sel28 (call'15 π'31)) (sel5 (call'15 π'31)) (sel22 (call'15 π'31)) (sel33 (call'15 π'31)) (sel19 (call'15 π'31)) (sel3 (call'15 π'31)) (sel6 (call'15 π'31)) (sel17 (call'15 π'31)) (sel32 (call'15 π'31)) (sel12 (call'15 π'31)) (sel14 (call'15 π'31)) (sel26 (call'15 π'31)) (sel1 (call'15 π'31)) (sel10 (call'15 π'31)) (sel16 (call'15 π'31)) (sel4 (call'15 π'31)) (sel25 (call'15 π'31)) (sel23 (call'15 π'31)) (sel29 (call'15 π'31)) (sel21 (call'15 π'31)) (sel20 (call'15 π'31)) (sel27 (call'15 π'31)) (sel9 (call'15 π'31)) (sel8 (call'15 π'31)) (sel31 (call'15 π'31)) (sel18 (call'15 π'31)) (sel13 (call'15 π'31)) (sel24 (call'15 π'31)) (sel30 (call'15 π'31)) π'31 ρ'104 ρ'105); [] -> [] }

main = Criterion.Main.defaultMain ((Criterion.Measurement.Types.bench (GHC.CString.unpackCString# "main"#) $ Criterion.Measurement.Types.whnf lam'9 (GHC.CString.unpackCString# "420"#)) : [])

call'18 flag_ds'3 _78 _79 _80 _cfε'4 _81 π'32 π'33 _82 π'34 _83 π'35 π'36 t'26 _cfε'5 π'37 _84 _85 π'38 π'39 π'40 _86 _87 ψ'9 _ccε'6 π'41 π'42 π'43 π'44 π'45 = let
  ψ'10 = (call'3 π'44 π'43)
  _cfε'6 = case flag_ds'3 of { True -> (case _84 of { True -> (case _82 of { True -> (let (:) _ arg = π'36 in arg); False -> (let (:) _ arg = π'40 in arg) }); False -> (case _87 of { True -> (case _81 of { True -> (case t'26 of { (:) ρ'126 ρ'127 -> (case _cfε'5 of { (:) ρ'128 ρ'129 -> (case _85 of { True -> (case _78 of { True -> (case π'34 of { (:) ρ'130 ρ'131 -> (let (:) _ arg = _79 in arg); [] -> (let (:) _ arg = ψ'9 in arg) }); False -> (case π'37 of { (:) ρ'132 ρ'133 -> (let (:) _ arg = _79 in arg); [] -> (let (:) _ arg = ψ'9 in arg) }) }); False -> (case _80 of { True -> (case _86 of { True -> (case _ccε'6 of { (:) ρ'134 ρ'135 -> (case _83 of { (:) ρ'136 ρ'137 -> (let (:) _ arg = _79 in arg); [] -> (let (:) _ arg = ψ'9 in arg) }); [] -> (case π'32 of { (:) ρ'138 ρ'139 -> (let (:) _ arg = _79 in arg); [] -> (let (:) _ arg = ψ'9 in arg) }) }); False -> (case π'38 of { (:) ρ'140 ρ'141 -> (let (:) _ arg = _79 in arg); [] -> (let (:) _ arg = ψ'9 in arg) }) }); False -> (case π'41 of { (:) ρ'142 ρ'143 -> (let (:) _ arg = _79 in arg); [] -> (let (:) _ arg = ψ'9 in arg) }) }) }); [] -> (let (:) _ arg = ψ'9 in arg) }); [] -> (let (:) _ arg = π'35 in arg) }); False -> (let (:) _ arg = π'39 in arg) }); False -> (let (:) _ arg = π'42 in arg) }) }); False -> (let (:) _ arg = _cfε'4 in arg) }
  in (,,,,,,,,) t'26 _87 π'33 ψ'10 _82 _81 _84 ((π'45 : (case flag_ds'3 of { True -> (case _84 of { True -> (case _82 of { True -> (let (:) arg _ = π'36 in arg); False -> (let (:) arg _ = π'40 in arg) }); False -> (case _87 of { True -> (case _81 of { True -> (case t'26 of { (:) ρ'106 ρ'107 -> (case _cfε'5 of { (:) ρ'108 ρ'109 -> (case _85 of { True -> (case _78 of { True -> (case π'34 of { (:) ρ'110 ρ'111 -> (let (:) arg _ = _79 in arg); [] -> (let (:) arg _ = ψ'9 in arg) }); False -> (case π'37 of { (:) ρ'112 ρ'113 -> (let (:) arg _ = _79 in arg); [] -> (let (:) arg _ = ψ'9 in arg) }) }); False -> (case _80 of { True -> (case _86 of { True -> (case _ccε'6 of { (:) ρ'114 ρ'115 -> (case _83 of { (:) ρ'116 ρ'117 -> (let (:) arg _ = _79 in arg); [] -> (let (:) arg _ = ψ'9 in arg) }); [] -> (case π'32 of { (:) ρ'118 ρ'119 -> (let (:) arg _ = _79 in arg); [] -> (let (:) arg _ = ψ'9 in arg) }) }); False -> (case π'38 of { (:) ρ'120 ρ'121 -> (let (:) arg _ = _79 in arg); [] -> (let (:) arg _ = ψ'9 in arg) }) }); False -> (case π'41 of { (:) ρ'122 ρ'123 -> (let (:) arg _ = _79 in arg); [] -> (let (:) arg _ = ψ'9 in arg) }) }) }); [] -> (let (:) arg _ = ψ'9 in arg) }); [] -> (let (:) arg _ = π'35 in arg) }); False -> (let (:) arg _ = π'39 in arg) }); False -> (let (:) arg _ = π'42 in arg) }) }); False -> (let (:) arg _ = _cfε'4 in arg) })) : (case _cfε'6 of { (:) ρ'124 ρ'125 -> sel8 (call'18 False undefined undefined undefined _cfε'6 undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) π'43 π'44 π'45); [] -> sel4 (call'18 False undefined undefined undefined _cfε'6 undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) π'43 π'44 π'45) })) ψ'10

call'16 _88 _89 = (call'6 (GHC.Enum.enumFromTo (1::Int) (_89 - Data.Foldable.length _88))) ++ _88

lam'5 _90 = \ds'6 -> ds'6 /= _90

alphabeticRule = lam'10

call'17 flag_ds'4'2 π'46 _91 π'47 _92 π'48 _93 π'49 π'50 _cfε'7 = (,) (call'8 flag_ds'4'2 _cfε'7 π'50 _91 π'49 _93 _92 π'47) (((call'16 (GHC.Show.show π'48) _93) ++ (let (:) arg _ = π'46 in arg)) : (case (let (:) _ arg = π'46 in arg) of { (:) ρ'144 ρ'145 -> sel2 (call'17 flag_ds'4'2 (let (:) _ arg = π'46 in arg) _91 π'47 _92 π'48 _93 π'49 π'50 _cfε'7); [] -> sel1 (call'17 flag_ds'4'2 (let (:) _ arg = π'46 in arg) _91 π'47 _92 π'48 _93 π'49 π'50 _cfε'7) }))

lam'9 = \regex -> Data.Foldable.foldl' lam (0::Int) (Data.Foldable.concat (call'23 regex))

call π'51 = let
  _94 = GHC.List.span (lam'6 (C# '-'#)) π'51
  _99 = GHC.List.span (lam'5 (C# '>'#)) (case _94 of { (,) ρ'150 ρ'151 -> (let (:) _ arg = ρ'151 in arg) })
  _101 = (call'13 (case _99 of { (,) ρ'148 ρ'149 -> ρ'148 }))
  _100 = (call'12 (case _94 of { (,) ρ'146 ρ'147 -> ρ'146 }))
  _95 = _100 < _101
  _96 = GHC.Enum.enumFromTo _100 _101
  _98 = max (Data.Foldable.length (GHC.Show.show _100)) (Data.Foldable.length (GHC.Show.show _101))
  _97 = GHC.Enum.enumFromThenTo _100 (_100 - (1::Int)) _101
  in (,,,,) _95 _96 _97 (sel2 (call'11 _97 _95 _96 _98 _99 (let (:) _ arg = (let (,) _ arg = _99 in arg) in arg))) (sel1 (call'11 _97 _95 _96 _98 _99 (let (:) _ arg = (let (,) _ arg = _99 in arg) in arg)))

call'19 _102 _103 _104 t'27 π'52 _105 t'28 _106 _107 _108 _109 _110 _111 _ccε'7 _112 _113 t'29 t'30 _114 _115 _116 π'53 = let
  _117 = π'53 : (case π'52 of { (:) ρ'152 ρ'153 -> (case _109 of { True -> (case _115 of { True -> (let (:) arg _ = t'29 in arg); False -> (let (:) arg _ = t'29 in arg) }); False -> (case _113 of { True -> (case _106 of { True -> (let (:) arg _ = t'28 in arg); False -> (let (:) arg _ = t'30 in arg) }); False -> _107 }) }); [] -> [] })
  _119 = sel5 (call'24 True (let (:) _ arg = undefined in arg) _102 _112 _103 (let (:) _ arg = t'29 in arg) _105 _106 _109 (let (:) _ arg = t'30 in arg) (let (:) _ arg = t'29 in arg) _114 _110 _113 π'52 (let (:) _ arg = t'28 in arg) _115 π'53)
  _118 = case sel4 (call'24 True (let (:) _ arg = undefined in arg) _102 _112 _103 (let (:) _ arg = t'29 in arg) _105 _106 _109 (let (:) _ arg = t'30 in arg) (let (:) _ arg = t'29 in arg) _114 _110 _113 π'52 (let (:) _ arg = t'28 in arg) _115 π'53) of { (:) ρ'174 ρ'175 -> (case sel7 (call'24 True (let (:) _ arg = undefined in arg) _102 _112 _103 (let (:) _ arg = t'29 in arg) _105 _106 _109 (let (:) _ arg = t'30 in arg) (let (:) _ arg = t'29 in arg) _114 _110 _113 π'52 (let (:) _ arg = t'28 in arg) _115 π'53) of { True -> (case sel6 (call'24 True (let (:) _ arg = undefined in arg) _102 _112 _103 (let (:) _ arg = t'29 in arg) _105 _106 _109 (let (:) _ arg = t'30 in arg) (let (:) _ arg = t'29 in arg) _114 _110 _113 π'52 (let (:) _ arg = t'28 in arg) _115 π'53) of { True -> (case (let (:) _ arg = t'29 in arg) of { (:) ρ'176 ρ'177 -> _119; [] -> [] }); False -> (case (let (:) _ arg = t'29 in arg) of { (:) ρ'178 ρ'179 -> _119; [] -> [] }) }); False -> (case sel1 (call'24 True (let (:) _ arg = undefined in arg) _102 _112 _103 (let (:) _ arg = t'29 in arg) _105 _106 _109 (let (:) _ arg = t'30 in arg) (let (:) _ arg = t'29 in arg) _114 _110 _113 π'52 (let (:) _ arg = t'28 in arg) _115 π'53) of { True -> (case sel3 (call'24 True (let (:) _ arg = undefined in arg) _102 _112 _103 (let (:) _ arg = t'29 in arg) _105 _106 _109 (let (:) _ arg = t'30 in arg) (let (:) _ arg = t'29 in arg) _114 _110 _113 π'52 (let (:) _ arg = t'28 in arg) _115 π'53) of { True -> (case (let (:) _ arg = t'28 in arg) of { (:) ρ'180 ρ'181 -> _119; [] -> [] }); False -> (case (let (:) _ arg = t'30 in arg) of { (:) ρ'182 ρ'183 -> _119; [] -> [] }) }); False -> (case sel2 (call'24 True (let (:) _ arg = undefined in arg) _102 _112 _103 (let (:) _ arg = t'29 in arg) _105 _106 _109 (let (:) _ arg = t'30 in arg) (let (:) _ arg = t'29 in arg) _114 _110 _113 π'52 (let (:) _ arg = t'28 in arg) _115 π'53) of { (:) ρ'184 ρ'185 -> _119; [] -> [] }) }) }); [] -> [] }
  _120 = _117 : _118
  _ccε'8 = case π'52 of { (:) ρ'154 ρ'155 -> (case _109 of { True -> (case _115 of { True -> (case _116 of { (:) ρ'156 ρ'157 -> (case t'29 of { (:) ρ'158 ρ'159 -> _120; [] -> [] }); [] -> [] }); False -> (case _111 of { (:) ρ'160 ρ'161 -> (case t'29 of { (:) ρ'162 ρ'163 -> _120; [] -> [] }); [] -> [] }) }); False -> (case _113 of { True -> (case _106 of { True -> (case _104 of { (:) ρ'164 ρ'165 -> (case t'28 of { (:) ρ'166 ρ'167 -> _120; [] -> [] }); [] -> [] }); False -> (case _108 of { (:) ρ'168 ρ'169 -> (case t'30 of { (:) ρ'170 ρ'171 -> _120; [] -> [] }); [] -> [] }) }); False -> (case t'27 of { (:) ρ'172 ρ'173 -> _120; [] -> [] }) }) }); [] -> _120 }
  in (,,,,,) _ccε'8 _117 _118 _119 _ccε'7 _ccε'8

call'22 π'54 π'55 t'31 ψ'11 π'56 _cfε'8 _121 _122 t'32 _123 _124 ψ'12 π'57 _125 _126 _127 _128 _129 _130 _131 _132 t'33 _133 t'34 π'58 _134 t'35 π'59 _135 _ccε'9 t'36 _ccε'10 _136 π'60 π'61 π'62 = let
  _137 = sel1 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62)
  _139 = sel8 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62)
  _140 = case sel9 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62) of { (:) ρ'210 ρ'211 -> (case sel2 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62) of { True -> (case sel7 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62) of { True -> (case (let (:) _ arg = t'34 in arg) of { (:) ρ'212 ρ'213 -> _139; [] -> _137 }); False -> (case (let (:) _ arg = t'34 in arg) of { (:) ρ'214 ρ'215 -> _139; [] -> _137 }) }); False -> (case sel5 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62) of { True -> (case sel10 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62) of { True -> (case (let (:) _ arg = t'36 in arg) of { (:) ρ'216 ρ'217 -> _139; [] -> _137 }); False -> (case sel4 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62) of { (:) ρ'218 ρ'219 -> (case sel3 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62) of { (:) ρ'220 ρ'221 -> _139; [] -> _137 }); [] -> (case (let (:) _ arg = ψ'12 in arg) of { (:) ρ'222 ρ'223 -> _139; [] -> _137 }) }) }); False -> (case (let (:) _ arg = t'31 in arg) of { (:) ρ'224 ρ'225 -> _139; [] -> _137 }) }) }); [] -> _137 }
  ψ'13 = (call'21 π'62 π'60)
  _138 = π'61 : (case π'54 of { (:) ρ'206 ρ'207 -> (case _136 of { True -> (case _126 of { True -> (let (:) arg _ = t'34 in arg); False -> (let (:) arg _ = t'34 in arg) }); False -> (case _132 of { True -> (case _131 of { True -> (let (:) arg _ = t'36 in arg); False -> (case t'35 of { (:) ρ'208 ρ'209 -> _128; [] -> (let (:) arg _ = ψ'12 in arg) }) }); False -> (let (:) arg _ = t'31 in arg) }) }); [] -> [] })
  _141 = _138 : _140
  _ccε'11 = case π'54 of { (:) ρ'186 ρ'187 -> (case _136 of { True -> (case _126 of { True -> (case _135 of { (:) ρ'188 ρ'189 -> (case t'34 of { (:) ρ'190 ρ'191 -> _141; [] -> ψ'13 }); [] -> ψ'13 }); False -> (case _129 of { (:) ρ'192 ρ'193 -> (case t'34 of { (:) ρ'194 ρ'195 -> _141; [] -> ψ'13 }); [] -> ψ'13 }) }); False -> (case _132 of { True -> (case _131 of { True -> (case _123 of { (:) ρ'196 ρ'197 -> (case t'36 of { (:) ρ'198 ρ'199 -> _141; [] -> ψ'13 }); [] -> ψ'13 }); False -> (case _133 of { (:) ρ'200 ρ'201 -> (case t'33 of { (:) ρ'202 ρ'203 -> _141; [] -> ψ'13 }); [] -> ψ'13 }) }); False -> (case t'32 of { (:) ρ'204 ρ'205 -> _141; [] -> ψ'13 }) }) }); [] -> _141 }
  in (,,,,,,,,,,,,) _ccε'9 (let (:) _ arg = t'36 in arg) _ccε'11 _138 (let (:) _ arg = t'34 in arg) (let (:) _ arg = t'31 in arg) (sel6 (call'25 True (let (:) _ arg = t'36 in arg) t'35 _125 π'55 (let (:) _ arg = t'34 in arg) (let (:) _ arg = undefined in arg) _cfε'8 π'54 (let (:) _ arg = t'34 in arg) (let (:) _ arg = ψ'12 in arg) _136 π'58 _134 _132 _127 _126 _124 _122 π'57 _130 π'56 _121 π'59 _131 ψ'11 (let (:) _ arg = t'31 in arg) _ccε'10 π'60 π'61 π'62)) _139 _140 ψ'13 _ccε'11 (let (:) _ arg = ψ'12 in arg) (let (:) _ arg = t'34 in arg)

lam'10 = \ds'7 -> case ((let (:) arg _ = (let (:) _ arg = ds'7 in arg) in arg) == C# '-'#) && (((let (:) arg _ = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg) in arg) == C# ']'#) && ((let (:) arg _ = ds'7 in arg) <= (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg))) of { True -> sel12 (call'20 (GHC.Enum.enumFromTo (let (:) arg _ = ds'7 in arg) (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg)) (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg) in arg)); False -> sel2 (call'26 (GHC.List.reverse (GHC.Enum.enumFromTo (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg) (let (:) arg _ = ds'7 in arg))) (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = ds'7 in arg) in arg) in arg) in arg)) }

call'2 flag_ds π'63 _142 ds'8 ds'9 = let
  π'64 = case flag_ds of { True -> (case _142 of { (,) ρ'226 ρ'227 -> (let (:) _ arg = π'63 in arg) }); False -> (let (:) _ arg = ds'9 in arg) }
  _147 = sel4 (call'14 π'64)
  _150 = sel19 (call'14 π'64)
  π'65 = case flag_ds of { True -> (case _142 of { (,) ρ'228 ρ'229 -> (let (:) arg _ = π'63 in arg) }); False -> (let (:) arg _ = ds'8 in arg) }
  _145 = sel2 (call'14 π'64)
  _153 = sel7 (call'14 π'64)
  _155 = sel5 (call'14 π'64)
  _148 = sel12 (call'14 π'64)
  t'37 = sel6 (call'14 π'64)
  t'40 = sel15 (call'14 π'64)
  _149 = sel18 (call'14 π'64)
  _151 = sel9 (call'14 π'64)
  _143 = sel11 (call'14 π'64)
  _156 = sel3 (call'14 π'64)
  _146 = sel10 (call'14 π'64)
  t'39 = sel14 (call'14 π'64)
  _ccε'12 = sel8 (call'14 π'64)
  _144 = sel13 (call'14 π'64)
  _157 = sel20 (call'14 π'64)
  t'38 = sel1 (call'14 π'64)
  _152 = sel16 (call'14 π'64)
  _154 = sel17 (call'14 π'64)
  in (,,,,,,,,,) (sel3 (call'19 _143 _144 _145 t'37 π'64 _146 t'38 _147 _148 _149 _150 _151 _152 _ccε'12 _153 _154 t'39 t'40 _155 _156 _157 π'65)) (sel4 (call'19 _143 _144 _145 t'37 π'64 _146 t'38 _147 _148 _149 _150 _151 _152 _ccε'12 _153 _154 t'39 t'40 _155 _156 _157 π'65)) _156 (sel5 (call'19 _143 _144 _145 t'37 π'64 _146 t'38 _147 _148 _149 _150 _151 _152 _ccε'12 _153 _154 t'39 t'40 _155 _156 _157 π'65)) (sel1 (call'19 _143 _144 _145 t'37 π'64 _146 t'38 _147 _148 _149 _150 _151 _152 _ccε'12 _153 _154 t'39 t'40 _155 _156 _157 π'65)) _154 (sel6 (call'19 _143 _144 _145 t'37 π'64 _146 t'38 _147 _148 _149 _150 _151 _152 _ccε'12 _153 _154 t'39 t'40 _155 _156 _157 π'65)) _150 _147 (sel2 (call'19 _143 _144 _145 t'37 π'64 _146 t'38 _147 _148 _149 _150 _151 _152 _ccε'12 _153 _154 t'39 t'40 _155 _156 _157 π'65))

call'26 _158 π'66 = let
  _159 = sel29 (call'15 π'66)
  π'67 = sel2 (call'15 π'66)
  _cfε'9 = sel5 (call'15 π'66)
  _162 = sel3 (call'15 π'66)
  _ccε'13 = sel18 (call'15 π'66)
  t'45 = sel9 (call'15 π'66)
  _167 = sel1 (call'15 π'66)
  t'41 = sel15 (call'15 π'66)
  _168 = sel10 (call'15 π'66)
  π'69 = sel32 (call'15 π'66)
  t'42 = sel19 (call'15 π'66)
  t'46 = sel13 (call'15 π'66)
  _164 = sel12 (call'15 π'66)
  _172 = sel27 (call'15 π'66)
  _174 = sel30 (call'15 π'66)
  _169 = sel16 (call'15 π'66)
  _161 = sel33 (call'15 π'66)
  π'70 = sel20 (call'15 π'66)
  ψ'14 = sel11 (call'15 π'66)
  _170 = sel4 (call'15 π'66)
  π'68 = sel28 (call'15 π'66)
  _173 = sel31 (call'15 π'66)
  _ccε'14 = sel24 (call'15 π'66)
  _165 = sel14 (call'15 π'66)
  ψ'15 = sel17 (call'15 π'66)
  t'43 = sel23 (call'15 π'66)
  t'44 = sel21 (call'15 π'66)
  _160 = sel22 (call'15 π'66)
  π'71 = sel8 (call'15 π'66)
  _163 = sel6 (call'15 π'66)
  _171 = sel25 (call'15 π'66)
  _166 = sel26 (call'15 π'66)
  in (,,,,,,,,,,,,,,,,,,,) (sel10 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) (case _158 of { (:) ρ'230 ρ'231 -> sel11 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 ρ'230 ρ'231); [] -> [] }) (sel7 (call'15 π'66)) (sel4 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) _170 (sel3 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) (sel12 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) (sel7 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) (sel6 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) _161 (sel13 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) (sel5 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) _ccε'13 _174 _171 (sel9 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) (sel1 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) _165 (sel8 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg))) (sel2 (call'22 π'66 π'67 t'41 ψ'14 π'68 _cfε'9 _160 _161 t'42 _162 _163 ψ'15 π'69 _164 _165 _166 _167 _168 _169 _170 _171 t'43 _159 t'44 π'70 _172 t'45 π'71 _173 _ccε'13 t'46 _ccε'14 _174 π'66 (let (:) arg _ = _158 in arg) (let (:) _ arg = _158 in arg)))

numericRule = lam'4

call'7 π'72 = case π'72 of { (:) ρ'232 ρ'233 -> C# '0'# : (call'7 ρ'233); [] -> [] }

call'25 flag_ds'2 π'73 t'47 _175 π'74 π'75 _cfε'10 _cfε'11 π'76 π'77 π'78 _176 π'79 _177 _178 _179 _180 _181 _182 π'80 _183 π'81 _184 π'82 _185 ψ'16 π'83 _ccε'15 π'84 π'85 π'86 = let
  ψ'17 = (call'21 π'86 π'84)
  _cfε'12 = case flag_ds'2 of { True -> (case _176 of { True -> (case _180 of { True -> (let (:) _ arg = π'77 in arg); False -> (let (:) _ arg = π'75 in arg) }); False -> (case _178 of { True -> (case _185 of { True -> (let (:) _ arg = π'73 in arg); False -> (case t'47 of { (:) ρ'254 ρ'255 -> (case _cfε'11 of { (:) ρ'256 ρ'257 -> (case _179 of { True -> (case _184 of { True -> (case π'80 of { (:) ρ'258 ρ'259 -> (let (:) _ arg = _181 in arg); [] -> (let (:) _ arg = ψ'16 in arg) }); False -> (case π'74 of { (:) ρ'260 ρ'261 -> (let (:) _ arg = _181 in arg); [] -> (let (:) _ arg = ψ'16 in arg) }) }); False -> (case _177 of { True -> (case _175 of { True -> (case π'79 of { (:) ρ'262 ρ'263 -> (let (:) _ arg = _181 in arg); [] -> (let (:) _ arg = ψ'16 in arg) }); False -> (case _ccε'15 of { (:) ρ'264 ρ'265 -> (case _183 of { (:) ρ'266 ρ'267 -> (let (:) _ arg = _181 in arg); [] -> (let (:) _ arg = ψ'16 in arg) }); [] -> (case π'82 of { (:) ρ'268 ρ'269 -> (let (:) _ arg = _181 in arg); [] -> (let (:) _ arg = ψ'16 in arg) }) }) }); False -> (case π'81 of { (:) ρ'270 ρ'271 -> (let (:) _ arg = _181 in arg); [] -> (let (:) _ arg = ψ'16 in arg) }) }) }); [] -> (let (:) _ arg = ψ'16 in arg) }); [] -> (let (:) _ arg = π'78 in arg) }) }); False -> (let (:) _ arg = π'83 in arg) }) }); False -> (let (:) _ arg = _cfε'10 in arg) }
  in (,,,,,,,,,) ψ'17 _176 _182 t'47 _178 ψ'17 _180 ((π'85 : (case flag_ds'2 of { True -> (case _176 of { True -> (case _180 of { True -> (let (:) arg _ = π'77 in arg); False -> (let (:) arg _ = π'75 in arg) }); False -> (case _178 of { True -> (case _185 of { True -> (let (:) arg _ = π'73 in arg); False -> (case t'47 of { (:) ρ'234 ρ'235 -> (case _cfε'11 of { (:) ρ'236 ρ'237 -> (case _179 of { True -> (case _184 of { True -> (case π'80 of { (:) ρ'238 ρ'239 -> (let (:) arg _ = _181 in arg); [] -> (let (:) arg _ = ψ'16 in arg) }); False -> (case π'74 of { (:) ρ'240 ρ'241 -> (let (:) arg _ = _181 in arg); [] -> (let (:) arg _ = ψ'16 in arg) }) }); False -> (case _177 of { True -> (case _175 of { True -> (case π'79 of { (:) ρ'242 ρ'243 -> (let (:) arg _ = _181 in arg); [] -> (let (:) arg _ = ψ'16 in arg) }); False -> (case _ccε'15 of { (:) ρ'244 ρ'245 -> (case _183 of { (:) ρ'246 ρ'247 -> (let (:) arg _ = _181 in arg); [] -> (let (:) arg _ = ψ'16 in arg) }); [] -> (case π'82 of { (:) ρ'248 ρ'249 -> (let (:) arg _ = _181 in arg); [] -> (let (:) arg _ = ψ'16 in arg) }) }) }); False -> (case π'81 of { (:) ρ'250 ρ'251 -> (let (:) arg _ = _181 in arg); [] -> (let (:) arg _ = ψ'16 in arg) }) }) }); [] -> (let (:) arg _ = ψ'16 in arg) }); [] -> (let (:) arg _ = π'78 in arg) }) }); False -> (let (:) arg _ = π'83 in arg) }) }); False -> (let (:) arg _ = _cfε'10 in arg) })) : (case _cfε'12 of { (:) ρ'252 ρ'253 -> sel8 (call'25 False (let (:) _ arg = undefined in arg) undefined undefined undefined (let (:) _ arg = undefined in arg) _cfε'12 undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) undefined π'84 π'85 π'86); [] -> sel1 (call'25 False (let (:) _ arg = undefined in arg) undefined undefined undefined (let (:) _ arg = undefined in arg) _cfε'12 undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined (let (:) _ arg = undefined in arg) undefined π'84 π'85 π'86) })) π'76 _185

call'12 t'48 = Data.Foldable.foldl lam'8 (0::Int) t'48

call' flag_ds' _cfε'13 π'87 _186 = let
  π'88 = case flag_ds' of { True -> (case _186 of { (,) ρ'272 ρ'273 -> (let (:) arg _ = π'87 in arg) }); False -> (let (:) arg _ = _cfε'13 in arg) }
  π'90 = case flag_ds' of { True -> (case _186 of { (,) ρ'280 ρ'281 -> (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = π'87 in arg) in arg) in arg) }); False -> (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = _cfε'13 in arg) in arg) in arg) }
  _187 = GHC.List.reverse (GHC.Enum.enumFromTo π'90 π'88)
  π'89 = case flag_ds' of { True -> (case _186 of { (,) ρ'278 ρ'279 -> (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = π'87 in arg) in arg) in arg) in arg) }); False -> (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = _cfε'13 in arg) in arg) in arg) in arg) }
  _188 = GHC.Enum.enumFromTo π'88 π'90
  _189 = ((case flag_ds' of { True -> (case _186 of { (,) ρ'274 ρ'275 -> (let (:) arg _ = (let (:) _ arg = π'87 in arg) in arg) }); False -> (let (:) arg _ = (let (:) _ arg = _cfε'13 in arg) in arg) }) == C# '-'#) && (((case flag_ds' of { True -> (case _186 of { (,) ρ'276 ρ'277 -> (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = π'87 in arg) in arg) in arg) in arg) }); False -> (let (:) arg _ = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = _cfε'13 in arg) in arg) in arg) in arg) }) == C# ']'#) && (π'88 <= π'90))
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) (sel14 (call'26 _187 π'89)) (sel18 (call'20 _188 π'89)) (sel1 (call'20 _188 π'89)) (sel6 (call'20 _188 π'89)) (sel7 (call'20 _188 π'89)) (sel3 (call'20 _188 π'89)) (sel15 (call'20 _188 π'89)) (sel7 (call'26 _187 π'89)) (sel9 (call'20 _188 π'89)) (sel5 (call'20 _188 π'89)) (sel15 (call'26 _187 π'89)) (sel3 (call'26 _187 π'89)) (sel1 (call'26 _187 π'89)) (sel17 (call'26 _187 π'89)) (sel9 (call'26 _187 π'89)) (sel10 (call'26 _187 π'89)) (sel13 (call'20 _188 π'89)) _188 (sel2 (call'20 _188 π'89)) (sel11 (call'26 _187 π'89)) (sel19 (call'26 _187 π'89)) (sel20 (call'26 _187 π'89)) (sel4 (call'20 _188 π'89)) (sel13 (call'26 _187 π'89)) (sel12 (call'26 _187 π'89)) (sel11 (call'20 _188 π'89)) (sel8 (call'26 _187 π'89)) (case _189 of { True -> sel12 (call'20 _188 π'89); False -> sel2 (call'26 _187 π'89) }) (sel16 (call'26 _187 π'89)) (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = (let (:) _ arg = _cfε'13 in arg) in arg) in arg) in arg) (sel17 (call'20 _188 π'89)) (sel20 (call'20 _188 π'89)) _187 (sel4 (call'26 _187 π'89)) (sel18 (call'26 _187 π'89)) (sel5 (call'26 _187 π'89)) (sel16 (call'20 _188 π'89)) (sel19 (call'20 _188 π'89)) (sel14 (call'20 _188 π'89)) (sel6 (call'26 _187 π'89)) (sel8 (call'20 _188 π'89)) (sel10 (call'20 _188 π'89)) _189

call'24 flag_ds' _cfε'14 _190 _191 _192 π'91 _193 _194 _195 π'92 π'93 _196 _197 _198 π'94 π'95 _199 π'96 = 
  let _cfε'15 = case flag_ds' of { True -> (case _195 of { True -> (case _199 of { True -> (let (:) _ arg = π'93 in arg); False -> (let (:) _ arg = π'91 in arg) }); False -> (case _198 of { True -> (case _194 of { True -> (let (:) _ arg = π'95 in arg); False -> (let (:) _ arg = π'92 in arg) }); False -> (case _193 of { True -> (case _190 of { True -> (let (:) _ arg = _196 in arg); False -> (let (:) _ arg = _196 in arg) }); False -> (case _192 of { True -> (case _191 of { True -> (let (:) _ arg = _196 in arg); False -> (let (:) _ arg = _196 in arg) }); False -> (let (:) _ arg = _196 in arg) }) }) }) }); False -> (let (:) _ arg = _cfε'14 in arg) } in
  (,,,,,,) _198 _197 _194 π'94 ((π'96 : (case flag_ds' of { True -> (case _195 of { True -> (case _199 of { True -> (let (:) arg _ = π'93 in arg); False -> (let (:) arg _ = π'91 in arg) }); False -> (case _198 of { True -> (case _194 of { True -> (let (:) arg _ = π'95 in arg); False -> (let (:) arg _ = π'92 in arg) }); False -> (case _193 of { True -> (case _190 of { True -> (let (:) arg _ = _196 in arg); False -> (let (:) arg _ = _196 in arg) }); False -> (case _192 of { True -> (case _191 of { True -> (let (:) arg _ = _196 in arg); False -> (let (:) arg _ = _196 in arg) }); False -> (let (:) arg _ = _196 in arg) }) }) }) }); False -> (let (:) arg _ = _cfε'14 in arg) })) : (case _cfε'15 of { (:) ρ'282 ρ'283 -> sel5 (call'24 False _cfε'15 undefined undefined undefined (let (:) _ arg = undefined in arg) undefined undefined undefined (let (:) _ arg = undefined in arg) (let (:) _ arg = undefined in arg) undefined undefined undefined undefined (let (:) _ arg = undefined in arg) undefined π'96); [] -> [] })) _199 _195

lam'7 acc' = \c' -> ord c' + (acc' * (31::Int))

call'5 π'97 = let
  _200 = (let (:) arg _ = π'97 in arg) == GHC.List.head (GHC.CString.unpackCString# "<"#)
  _201 = (let (:) arg _ = π'97 in arg) == GHC.List.head (GHC.CString.unpackCString# "["#)
  in (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) (sel32 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel18 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel43 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel30 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel17 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel26 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel10 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel14 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel6 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel41 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel3 (call (let (:) _ arg = π'97 in arg))) (sel7 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel37 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel19 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel39 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel1 (call (let (:) _ arg = π'97 in arg))) (sel38 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel5 (call'2 False undefined undefined π'97 π'97)) (sel42 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel13 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel23 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (case π'97 of { (:) ρ'284 ρ'285 -> (case _200 of { True -> sel4 (call ρ'285); False -> (case _201 of { True -> sel28 (call' False ρ'285 (let (:) _ arg = undefined in arg) undefined); False -> sel7 (call'2 False undefined undefined π'97 π'97) }) }); [] -> [] : [] }) (sel4 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel31 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel9 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel5 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel5 (call (let (:) _ arg = π'97 in arg))) (sel40 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) _201 (sel3 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel2 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) (sel33 (call' False (let (:) _ arg = π'97 in arg) (let (:) _ arg = undefined in arg) undefined)) _200 (sel2 (call (let (:) _ arg = π'97 in arg)))

lam'6 _202 = \ds'10 -> ds'10 /= _202

expand = lam'2

call'10 π'98 _203 = let
  π'99 = case _203 of { (,) ρ'286 ρ'287 -> (let (:) arg _ = π'98 in arg) }
  π'100 = case _203 of { (,) ρ'288 ρ'289 -> (let (:) _ arg = π'98 in arg) }
  in (,,,,,,,,,,,,,,) (sel32 (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'98 in arg) _203)) (sel39 (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'98 in arg) _203)) (sel1 (call π'100)) (sel38 (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'98 in arg) _203)) (sel43 (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'98 in arg) _203)) (sel18 (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'98 in arg) _203)) (sel5 (call'2 True π'98 _203 undefined undefined)) (sel5 (call π'100)) (sel40 (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'98 in arg) _203)) (sel3 (call π'100)) (π'99 == GHC.List.head (GHC.CString.unpackCString# "["#)) (sel3 (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'98 in arg) _203)) (sel33 (call' True (let (:) _ arg = undefined in arg) (let (:) _ arg = π'98 in arg) _203)) (π'99 == GHC.List.head (GHC.CString.unpackCString# "<"#)) (sel2 (call π'100))

call'23 regex' = case regex' of { (:) ρ'290 ρ'291 -> (case ρ'290 == GHC.List.head (GHC.CString.unpackCString# "<"#) of { True -> sel4 (call ρ'291); False -> (case ρ'290 == GHC.List.head (GHC.CString.unpackCString# "["#) of { True -> sel28 (call' False ρ'291 (let (:) _ arg = undefined in arg) undefined); False -> sel7 (call'2 False undefined undefined regex' regex') }) }); [] -> [] : [] }
