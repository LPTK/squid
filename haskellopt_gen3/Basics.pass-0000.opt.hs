-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  55
-- Incl. one-shot:   2
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 1645; Boxes: 415; Branches: 372
-- Apps: 296; Lams: 8

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Basics (foo_0,foo_1,foo_2,foo_3,fTest4,fTest3,fTest2,fTest1,fTest0,f,gTest6,gTest5,gTest4,gTest3,gTest2,gTest1,gTest0,g,hello1) where

import Control.Exception.Base
import GHC.CString
import GHC.Classes
import GHC.Num
import GHC.Prim
import GHC.Real
import GHC.Types

call _0 = _0

lam = \x -> (call' (x + (1::Int)) x) - (call'2 x x)

call'3 _1 = _1

call'4 _2 x' = x' * _2

gTest3 = ((2::Int) * (3::Int)) * (4::Int)

fTest4 = (call'5 (call'6 (66::Int))) + (call'7 (call'8 (77::Int)))

call'9 _3 = _3 * _3

call'10 _4 x'2 = x'2 * _4

fTest1 = (call'11 (call'12 (33::Int)))

call'13 _5 x'3 = x'3 * _5

call'14 _6 = _6 * _6

call'15 y x'4 = x'4 * y

call'16 _7 = ((11::Int) * _7) * (_7 * (22::Int))

call'17 _8 = _8

lam' = \x'5 -> (lam'2 x'5)

gTest0 = ((2::Int) * (3::Int)) * (4::Int)

call'11 _9 = _9 * _9

lam'3 = \x'6 -> 
  let _10 = x'6 * (2::Int) in
  _10 + _10

foo_1 = lam'4

call'18 _11 = _11

call'19 z x'7 = x'7 * z

call'8 _12 = _12 * _12

call'20 _13 = ((11::Int) * _13) * (_13 * (22::Int))

call' _14 x'8 = 
  let _15 = _14 * x'8 in
  _15 ^ _15

gTest6 = ((44::Int) * (33::Int)) * (11::Int)

call'6 _16 = _16 * _16

call'21 _17 = _17

call'22 _18 x'9 = x'9 * _18

hello1 = C# 'H'# : GHC.CString.unpackCString# "ello"#

foo_0 = lam'3

f = lam'5

foo_3 = lam

gTest4 = ((2::Int) * (3::Int)) * ((4::Int) * (5::Int))

call'23 _19 = _19

call'24 _20 x'10 = x'10 * _20

call'5 _21 = _21 * _21

call'25 _22 = _22 * _22

call'26 _23 = _23

lam'6 = \x'11 -> (x'11 + (1::Int)) * x'11

call'27 _24 = _24 * _24

call'28 _25 = _25

lam'2 x'12 = \y' -> x'12 * y'

call'29 _26 = _26 * _26

call'30 _27 x'13 = x'13 * _27

call'31 _28 x'14 = x'14 * _28

gTest1 = (4::Int) * ((2::Int) * (3::Int))

call'32 _29 = _29

call'33 _30 y'2 = _30 * y'2

fTest2 = (call'34 (44::Int)) + (call'35 (call'9 (55::Int)))

call'35 _31 = _31 * _31

call'36 _32 = _32

call'37 z' = z'

fTest3 = (call'29 (call'38 (66::Int))) * (call'14 (call'27 (77::Int)))

gTest2 = (lam'2 ((2::Int) * (3::Int)))

lam'5 = \x'15 -> x'15 * x'15

fTest0 = (call'39 (11::Int)) * (call'25 (22::Int))

call'40 _33 x'16 = x'16 * _33

call'41 _34 x'17 = x'17 * _34

g = lam'

call'42 _35 = _35

call'43 _36 y'3 = _36 * y'3

call'44 _37 = _37

call'34 _38 = _38 * _38

call'45 _39 x'18 = x'18 * _39

call'46 _40 = _40

foo_2 = lam'6

call'2 x'19 x'20 = 
  let _41 = x'19 * x'20 in
  _41 ^ _41

call'39 _42 = _42 * _42

lam'4 = \x'21 -> (x'21 + (1::Int)) * (2::Int)

call'38 _43 = _43 * _43

call'47 _44 = _44

gTest5 = (call'16 (30::Int)) + (call'20 (40::Int))

call'48 x'22 = x'22

call'49 _45 x'23 = x'23 * _45

call'50 _46 x'24 = x'24 * _46

call'51 _47 = (lam'2 _47)

call'7 _48 = _48 * _48

call'12 _49 = _49 * _49
