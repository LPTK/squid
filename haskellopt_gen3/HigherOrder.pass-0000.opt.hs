-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  58
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 1270; Boxes: 380; Branches: 320
-- Apps: 257; Lams: 18

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module HigherOrder (hTest5,hTest4,hTest3,h,gTest1,gTest0,g,m1,iTest2,iTest1,iTest0,i,f0,ls1,lol) where

import GHC.Num
import GHC.Types

lam x = \y -> x + y

call = (11::Int)

lam' flag_f' flag_f'' flag_f''2 flag_f''3 f'2 = \x' -> (case flag_f''2 of { True -> (call' (1::Int)); False -> (case flag_f'' of { True -> (call'2 (1::Int)); False -> (case flag_f''3 of { True -> (call'3 (1::Int)); False -> (case flag_f' of { True -> (call'4 (1::Int)); False -> f'2 (lam'2 (1::Int)) }) }) }) }) + (case flag_f''2 of { True -> (call'5 (2::Int)); False -> (case flag_f'' of { True -> (call'6 (2::Int)); False -> (case flag_f''3 of { True -> (call'7 (2::Int)); False -> (case flag_f' of { True -> (call'8 (2::Int)); False -> f'2 (lam'3 (2::Int)) }) }) }) })

call'9 _0 _1 = _0 * _1

call'3 _2 = ((11::Int) + _2) + ((22::Int) + _2)

hTest4 = (call'10 (1::Int)) - (call'11 (2::Int))

iTest0 = call'12

call'13 _3 = _3 - (1::Int)

call'14 _4 _5 = _4 + _5

gTest1 = (call'15 (call'16 (4::Int)))

call'4 _6 = ((11::Int) + _6) + ((22::Int) + _6)

call'17 _7 = _7 (2::Int) * _7 (3::Int)

call'18 = (call' (1::Int)) + (call'5 (2::Int))

call'19 _8 _9 = _8 + _9

call'20 _10 _11 = _10 * _11

lam'4 = \f' -> (lam' False False False False f')

ls1 = (+) (call'21 (11::Int))

call'22 x'2 = x'2 - (1::Int)

lam'5 = \f -> (lam'6 False False False f)

call'23 _12 _13 = _12 * _13

call'8 _14 = ((11::Int) * _14) + ((22::Int) * _14)

call'2 _15 = ((11::Int) + _15) + ((22::Int) + _15)

call'16 _16 = (call'24 _16) + (call'13 (3::Int))

call'25 x'3 = (x'3 - (2::Int)) * (x'3 - (3::Int))

m1 = lam'7

call'26 _17 = _17 * (call'25 _17)

call'7 _18 = ((11::Int) * _18) + ((22::Int) * _18)

lam'8 = \x'4 -> (lam x'4)

call'27 _19 _20 = _19 * _20

g = lam'5

lam'9 = \f'3 -> f'3 (2::Int) * f'3 (3::Int)

hTest5 = call'28

iTest1 = call'29

call'21 x'5 = x'5 + (22::Int)

call'30 _21 _22 = _21 + _22

call'31 _23 x'6 = x'6 - _23

call'12 = (lam' False False False True undefined)

call'32 x'7 = x'7 - (1::Int)

lam'7 = \x'8 -> x'8 - (1::Int)

call' _24 = ((11::Int) + _24) + ((22::Int) + _24)

lam'3 _25 = \ds -> ds * _25

call'11 _26 = ((2::Int) * _26) * ((3::Int) * _26)

call'33 _27 _28 = _27 * _28

call'34 _29 _30 = _29 + _30

f0 = lam'10

call'35 _31 _32 = _31 + _32

call'36 _33 = (call'32 _33) + (call'37 (3::Int))

h = lam'9

call'38 _34 _35 = _34 * _35

call'39 _36 = _36 * (call'25 _36)

call'15 _37 = (call'22 _37) + (call'40 (3::Int))

call'5 _38 = ((11::Int) * _38) + ((22::Int) * _38)

hTest3 = (call'17 ((+) (1::Int))) - (call'41 ((*) (2::Int)))

lam'2 _39 = \ds' -> ds' + _39

i = lam'4

gTest0 = (call'36 (2::Int))

call'42 _40 _41 = _40 * _41

call'6 _42 = ((11::Int) * _42) + ((22::Int) * _42)

lam'10 = \f'4 -> f'4 (11::Int) + f'4 (22::Int)

lol = lam'8

call'43 _43 _44 = _43 + _44

call'44 _45 _46 = _45 * _46

call'41 _47 = _47 (2::Int) * _47 (3::Int)

call'45 _48 _49 = _48 + _49

call'46 _50 _51 = _50 * _51

call'29 = (call'2 (1::Int)) + (call'6 (2::Int))

call'47 _52 x'9 = x'9 - _52

call'40 _53 = _53 - (1::Int)

lam'6 flag_f flag_f' flag_f'2 f'5 = \x'10 -> (case flag_f' of { True -> (call'22 x'10); False -> (case flag_f'2 of { True -> (call'24 x'10); False -> (case flag_f of { True -> (call'32 x'10); False -> f'5 x'10 }) }) }) + (case flag_f' of { True -> (call'40 (3::Int)); False -> (case flag_f'2 of { True -> (call'13 (3::Int)); False -> (case flag_f of { True -> (call'37 (3::Int)); False -> f'5 (3::Int) }) }) })

call'24 x'11 = x'11 - (1::Int)

call'48 _54 _55 = _54 + _55

iTest2 = call'18

call'49 _56 _57 = _56 + _57

call'50 _58 _59 = _58 + _59

call'10 _60 = ((2::Int) + _60) * ((3::Int) + _60)

call'51 _61 _62 = _61 * _62

call'37 _63 = _63 - (1::Int)

call'28 = (call'39 (2::Int)) * (call'26 (3::Int))
