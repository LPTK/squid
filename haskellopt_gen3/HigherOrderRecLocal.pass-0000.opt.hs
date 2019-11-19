-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  39
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 1916; Boxes: 614; Branches: 675
-- Apps: 285; Lams: 38

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module HigherOrderRecLocal (foo_6_5,foo_6,foo_5_10,foo_5,foo_4,foo_3,foo_2,foo_1,foo_0,foo) where

import Data.Tuple.Select
import GHC.Base
import GHC.Classes
import GHC.List
import GHC.Num
import GHC.Real
import GHC.Types

call flag_f flag_f' flag_f'2 flag_f'3 s k'4 k'5 k'6 k'7 k'8 k'9 k'10 k'11 = let
  _0 = s * (2::Int)
  _1 = s + (1::Int)
  in s : (case mod s (2::Int) == (0::Int) of { True -> (case flag_f'2 of { True -> (call' flag_f' flag_f'3 flag_f'2 flag_f _1 k'4 k'5 k'6 k'6 k'4 k'9 k'10 k'11); False -> (case flag_f'3 of { True -> (call'2 flag_f' flag_f'2 flag_f flag_f'3 _1 k'4 k'5 k'10 k'6 k'9 k'10 k'11); False -> (case flag_f of { True -> (call'3 flag_f' flag_f'3 flag_f flag_f'2 _1 k'4 k'5 k'6 k'11 k'5 k'9 k'10 k'11); False -> (case flag_f' of { True -> (call'4 flag_f'2 flag_f'3 flag_f flag_f' _1 k'4 k'5 k'6 k'9 k'10 k'9 k'11); False -> k'8 _1 }) }) }) }); False -> (case flag_f'2 of { True -> (call'5 flag_f flag_f' flag_f'2 flag_f'3 _0 k'4 k'11 k'6 k'6 k'4 k'9 k'10 k'5); False -> (case flag_f'3 of { True -> (call'6 flag_f'3 flag_f' flag_f flag_f'2 _0 k'4 k'11 k'5 k'6 k'9 k'10 k'10); False -> (case flag_f of { True -> (call'7 flag_f'3 flag_f' flag_f flag_f'2 _0 k'4 k'11 k'6 k'11 k'5 k'9 k'10 k'5); False -> (case flag_f' of { True -> (call'8 flag_f'2 flag_f'3 flag_f flag_f' _0 k'4 k'5 k'6 k'9 k'10 k'9 k'11); False -> k'7 _0 }) }) }) }) })

foo_5_10 = GHC.List.take (10::Int) (call'9 (sel3 call'10) (sel1 call'10) (sel7 call'10) (sel4 call'10) (sel6 call'10) (sel2 call'10))

lam flag_f_k flag_f_k' flag_f'4 flag_f'5 flag_f_k'2 flag_f'6 flag_f'7 flag_f_k'3 k'12 k'13 k'14 k'15 k'16 k'17 k'18 k'19 = \s' -> let
  _3 = s' * (2::Int)
  _2 = s' + (1::Int)
  in s' : (case mod s' (2::Int) == (0::Int) of { True -> (case flag_f_k'2 of { True -> (call' flag_f'5 flag_f'6 flag_f'4 flag_f'7 _2 k'12 k'19 k'14 k'14 k'12 k'17 k'18 k'13); False -> (case flag_f_k' of { True -> (call'2 flag_f'5 flag_f'4 flag_f'7 flag_f'6 _2 k'12 k'19 k'18 k'14 k'17 k'18 k'13); False -> (case flag_f_k'3 of { True -> (call'3 flag_f'5 flag_f'6 flag_f'7 flag_f'4 _2 k'12 k'19 k'14 k'13 k'19 k'17 k'18 k'13); False -> (case flag_f_k of { True -> (call'4 flag_f'4 flag_f'6 flag_f'7 flag_f'5 _2 k'12 k'19 k'14 k'17 k'18 k'17 k'13); False -> k'16 _2 }) }) }) }); False -> (case flag_f_k'2 of { True -> (call'5 flag_f'7 flag_f'5 flag_f'4 flag_f'6 _3 k'12 k'13 k'14 k'14 k'12 k'17 k'18 k'19); False -> (case flag_f_k' of { True -> (call'6 flag_f'6 flag_f'5 flag_f'7 flag_f'4 _3 k'12 k'13 k'19 k'14 k'17 k'18 k'18); False -> (case flag_f_k'3 of { True -> (call'7 flag_f'6 flag_f'5 flag_f'7 flag_f'4 _3 k'12 k'13 k'14 k'13 k'19 k'17 k'18 k'19); False -> (case flag_f_k of { True -> (call'8 flag_f'4 flag_f'6 flag_f'7 flag_f'5 _3 k'12 k'19 k'14 k'17 k'18 k'17 k'13); False -> k'15 _3 }) }) }) }) })

call'9 k'20 k'21 k'22 k'23 k'24 k'25 = (23::Int) : ((call'11 False False True False ((23::Int) + (1::Int)) k'20 k'25 k'22 k'21 k'25 k'23 k'24 k'21) ++ (call'12 False False True False ((23::Int) * (2::Int)) k'20 k'21 k'22 k'21 k'25 k'23 k'24 k'25))

lam' flag_f_k'3 flag_f_k'3' flag_f'8 flag_f'9 flag_f_k'3'2 flag_f'10 flag_f'11 flag_f_k'3'3 k'26 k'27 k'28 k'29 k'30 k'31 k'32 = \s'2 -> s'2 : (case flag_f_k'3'2 of { True -> (call'13 flag_f'9 flag_f'10 flag_f'8 flag_f'11 s'2 k'26 k'32 k'29 k'27 k'32 k'30 k'31 k'27); False -> (case flag_f_k'3' of { True -> (call'14 flag_f'11 flag_f'9 flag_f'8 flag_f'10 s'2 k'26 k'32 k'29 k'30 k'31 k'30 k'27); False -> (case flag_f_k'3'3 of { True -> (call flag_f'8 flag_f'9 flag_f'10 flag_f'11 s'2 k'26 k'32 k'29 k'29 k'26 k'30 k'31 k'27); False -> (case flag_f_k'3 of { True -> (call'15 flag_f'11 flag_f'9 flag_f'8 flag_f'10 s'2 k'26 k'27 k'32 k'29 k'30 k'31 k'31); False -> k'28 s'2 }) }) }) })

call'16 flag_f'12 flag_f'13 flag_f'14 flag_f'15 _4 k'33 k'34 k'35 k'36 k'37 k'38 k'39 = 
  let _5 = _4 + (1::Int) in
  _4 : (case flag_f'14 of { True -> (call'17 flag_f'13 flag_f'15 flag_f'14 flag_f'12 _5 k'33 k'34 k'35 k'39 k'34 k'36 k'37 k'39); False -> (case flag_f'15 of { True -> (call'18 flag_f'12 flag_f'13 flag_f'14 flag_f'15 _5 k'33 k'34 k'35 k'36 k'37 k'36 k'39); False -> (case flag_f'12 of { True -> (call'19 flag_f'14 flag_f'15 flag_f'12 flag_f'13 _5 k'33 k'34 k'35 k'35 k'33 k'36 k'37 k'39); False -> (case flag_f'13 of { True -> (call'20 flag_f'13 flag_f'15 flag_f'14 flag_f'12 _5 k'33 k'34 k'37 k'35 k'36 k'37 k'39); False -> k'38 _5 }) }) }) })

call'17 flag_f'16 flag_f'17 flag_f'18 flag_f'19 _6 k'40 k'41 k'42 k'43 k'44 k'45 k'46 k'47 = let
  _7 = _6 * (2::Int)
  _8 = _6 + (1::Int)
  in _6 : ((case flag_f'18 of { True -> (call'11 flag_f'17 flag_f'19 flag_f'18 flag_f'16 _8 k'40 k'41 k'42 k'47 k'41 k'45 k'46 k'47); False -> (case flag_f'19 of { True -> (call'21 flag_f'17 flag_f'18 flag_f'19 flag_f'16 _8 k'40 k'41 k'42 k'42 k'40 k'45 k'46 k'47); False -> (case flag_f'16 of { True -> (call'22 flag_f'17 flag_f'16 flag_f'18 flag_f'19 _8 k'40 k'41 k'46 k'42 k'45 k'46 k'47); False -> (case flag_f'17 of { True -> (call'16 flag_f'19 flag_f'16 flag_f'18 flag_f'17 _8 k'40 k'41 k'42 k'45 k'46 k'45 k'47); False -> k'44 _8 }) }) }) }) ++ (case flag_f'16 of { True -> (call'23 flag_f'16 flag_f'17 flag_f'18 flag_f'19 _7 k'40 k'47 k'41 k'42 k'45 k'46 k'46); False -> (case flag_f'17 of { True -> (call'24 flag_f'19 flag_f'16 flag_f'18 flag_f'17 _7 k'40 k'41 k'42 k'45 k'46 k'45 k'47); False -> (case flag_f'18 of { True -> (call'12 flag_f'16 flag_f'17 flag_f'18 flag_f'19 _7 k'40 k'47 k'42 k'47 k'41 k'45 k'46 k'41); False -> (case flag_f'19 of { True -> (call'25 flag_f'18 flag_f'17 flag_f'19 flag_f'16 _7 k'40 k'47 k'42 k'42 k'40 k'45 k'46 k'41); False -> k'43 _7 }) }) }) }))

foo_3 = call'26

call'24 flag_f'20 flag_f'21 flag_f'22 flag_f'23 _9 k'48 k'49 k'50 k'51 k'52 k'53 k'54 = 
  let _10 = _9 + (1::Int) in
  _9 : (case flag_f'22 of { True -> (call'17 flag_f'21 flag_f'23 flag_f'22 flag_f'20 _10 k'48 k'49 k'50 k'54 k'49 k'51 k'52 k'54); False -> (case flag_f'23 of { True -> (call'18 flag_f'20 flag_f'21 flag_f'22 flag_f'23 _10 k'48 k'49 k'50 k'51 k'52 k'51 k'54); False -> (case flag_f'20 of { True -> (call'19 flag_f'22 flag_f'23 flag_f'20 flag_f'21 _10 k'48 k'49 k'50 k'50 k'48 k'51 k'52 k'54); False -> (case flag_f'21 of { True -> (call'20 flag_f'21 flag_f'23 flag_f'22 flag_f'20 _10 k'48 k'49 k'52 k'50 k'51 k'52 k'54); False -> k'53 _10 }) }) }) })

call'4 flag_f'24 flag_f'25 flag_f'26 flag_f'27 _11 k'55 k'56 k'57 k'58 k'59 k'60 k'61 = 
  let _12 = _11 + (1::Int) in
  _11 : (case flag_f'26 of { True -> (call'17 flag_f'25 flag_f'27 flag_f'26 flag_f'24 _12 k'55 k'56 k'57 k'61 k'56 k'58 k'59 k'61); False -> (case flag_f'27 of { True -> (call'18 flag_f'24 flag_f'25 flag_f'26 flag_f'27 _12 k'55 k'56 k'57 k'58 k'59 k'58 k'61); False -> (case flag_f'24 of { True -> (call'19 flag_f'26 flag_f'27 flag_f'24 flag_f'25 _12 k'55 k'56 k'57 k'57 k'55 k'58 k'59 k'61); False -> (case flag_f'25 of { True -> (call'20 flag_f'25 flag_f'27 flag_f'26 flag_f'24 _12 k'55 k'56 k'59 k'57 k'58 k'59 k'61); False -> k'60 _12 }) }) }) })

foo_0 = call'27

call'6 flag_f'28 flag_f'29 flag_f'30 flag_f'31 _13 k'62 k'63 k'64 k'65 k'66 k'67 k'68 = _13 : (case flag_f'30 of { True -> (call'13 flag_f'29 flag_f'31 flag_f'30 flag_f'28 _13 k'62 k'64 k'65 k'63 k'64 k'66 k'67 k'63); False -> (case flag_f'29 of { True -> (call'14 flag_f'28 flag_f'29 flag_f'30 flag_f'31 _13 k'62 k'64 k'65 k'66 k'67 k'66 k'63); False -> (case flag_f'31 of { True -> (call flag_f'30 flag_f'29 flag_f'31 flag_f'28 _13 k'62 k'64 k'65 k'65 k'62 k'66 k'67 k'63); False -> (case flag_f'28 of { True -> (call'15 flag_f'28 flag_f'29 flag_f'30 flag_f'31 _13 k'62 k'63 k'64 k'65 k'66 k'67 k'67); False -> k'68 _13 }) }) }) })

call'28 flag_f'32 flag_f'33 flag_f'34 flag_f'35 k'69 _14 k'70 k'71 k'72 k'73 k'74 = (,) (lam' flag_f'32 flag_f'33 flag_f'34 flag_f'33 flag_f'34 flag_f'35 flag_f'32 flag_f'35 k'69 k'72 _14 k'73 k'74 k'71 k'70) _14

call' flag_f'36 flag_f'37 flag_f'38 flag_f'39 _15 k'75 k'76 k'77 k'78 k'79 k'80 k'81 k'82 = let
  _16 = _15 + (1::Int)
  _17 = _15 * (2::Int)
  in _15 : (case mod _15 (2::Int) == (0::Int) of { True -> (case flag_f'38 of { True -> (call' flag_f'36 flag_f'37 flag_f'38 flag_f'39 _16 k'75 k'76 k'77 k'77 k'75 k'80 k'81 k'82); False -> (case flag_f'37 of { True -> (call'2 flag_f'36 flag_f'38 flag_f'39 flag_f'37 _16 k'75 k'76 k'81 k'77 k'80 k'81 k'82); False -> (case flag_f'39 of { True -> (call'3 flag_f'36 flag_f'37 flag_f'39 flag_f'38 _16 k'75 k'76 k'77 k'82 k'76 k'80 k'81 k'82); False -> (case flag_f'36 of { True -> (call'4 flag_f'38 flag_f'37 flag_f'39 flag_f'36 _16 k'75 k'76 k'77 k'80 k'81 k'80 k'82); False -> k'79 _16 }) }) }) }); False -> (case flag_f'38 of { True -> (call'5 flag_f'39 flag_f'36 flag_f'38 flag_f'37 _17 k'75 k'82 k'77 k'77 k'75 k'80 k'81 k'76); False -> (case flag_f'37 of { True -> (call'6 flag_f'37 flag_f'36 flag_f'39 flag_f'38 _17 k'75 k'82 k'76 k'77 k'80 k'81 k'81); False -> (case flag_f'39 of { True -> (call'7 flag_f'37 flag_f'36 flag_f'39 flag_f'38 _17 k'75 k'82 k'77 k'82 k'76 k'80 k'81 k'76); False -> (case flag_f'36 of { True -> (call'8 flag_f'38 flag_f'37 flag_f'39 flag_f'36 _17 k'75 k'76 k'77 k'80 k'81 k'80 k'82); False -> k'78 _17 }) }) }) }) })

call'29 = let
  k'83 = sel2 ret
  k'87 = sel3 ret'2
  k'88 = sel1 ret'
  k'85 = sel2 (call'28 False True False False k'84 undefined k'83 k'85 k'86 k'87 k'88)
  ret = (call'32 False True False False k'84 k'83 k'85 k'86 k'87 undefined k'88 undefined)
  k'86 = sel3 ret
  ret'2 = (call'31 False True False False k'84 undefined undefined k'85 k'83 k'86 k'87 k'88)
  k'84 = sel1 ret'2
  ret' = (call'30 False False False True k'84 k'83 k'85 k'86 k'87 k'88 undefined)
  λ = sel2 ret'
  in λ

call'21 flag_f'40 flag_f'41 flag_f'42 flag_f'43 _18 k'89 k'90 k'91 k'92 k'93 k'94 k'95 k'96 = let
  _19 = _18 * (2::Int)
  _20 = _18 + (1::Int)
  in _18 : (case mod _18 (2::Int) == (0::Int) of { True -> (case flag_f'42 of { True -> (call' flag_f'40 flag_f'43 flag_f'42 flag_f'41 _20 k'89 k'90 k'91 k'91 k'89 k'94 k'95 k'96); False -> (case flag_f'43 of { True -> (call'2 flag_f'40 flag_f'42 flag_f'41 flag_f'43 _20 k'89 k'90 k'95 k'91 k'94 k'95 k'96); False -> (case flag_f'41 of { True -> (call'3 flag_f'40 flag_f'43 flag_f'41 flag_f'42 _20 k'89 k'90 k'91 k'96 k'90 k'94 k'95 k'96); False -> (case flag_f'40 of { True -> (call'4 flag_f'42 flag_f'43 flag_f'41 flag_f'40 _20 k'89 k'90 k'91 k'94 k'95 k'94 k'96); False -> k'93 _20 }) }) }) }); False -> (case flag_f'42 of { True -> (call'5 flag_f'41 flag_f'40 flag_f'42 flag_f'43 _19 k'89 k'96 k'91 k'91 k'89 k'94 k'95 k'90); False -> (case flag_f'43 of { True -> (call'6 flag_f'43 flag_f'40 flag_f'41 flag_f'42 _19 k'89 k'96 k'90 k'91 k'94 k'95 k'95); False -> (case flag_f'41 of { True -> (call'7 flag_f'43 flag_f'40 flag_f'41 flag_f'42 _19 k'89 k'96 k'91 k'96 k'90 k'94 k'95 k'90); False -> (case flag_f'40 of { True -> (call'8 flag_f'42 flag_f'43 flag_f'41 flag_f'40 _19 k'89 k'90 k'91 k'94 k'95 k'94 k'96); False -> k'92 _19 }) }) }) }) })

foo_1 = id

foo_6 = sel3 call'33

lam'2 = \x -> GHC.List.take (5::Int) (call'34 x (sel4 call'33) (sel2 call'33) (sel7 call'33) (sel5 call'33) (sel6 call'33) (sel1 call'33))

call'11 flag_f'44 flag_f'45 flag_f'46 flag_f'47 _21 k'97 k'98 k'99 k'100 k'101 k'102 k'103 k'104 = let
  _22 = _21 * (2::Int)
  _23 = _21 + (1::Int)
  in _21 : ((case flag_f'46 of { True -> (call'11 flag_f'44 flag_f'45 flag_f'46 flag_f'47 _23 k'97 k'98 k'99 k'104 k'98 k'102 k'103 k'104); False -> (case flag_f'45 of { True -> (call'21 flag_f'44 flag_f'46 flag_f'45 flag_f'47 _23 k'97 k'98 k'99 k'99 k'97 k'102 k'103 k'104); False -> (case flag_f'47 of { True -> (call'22 flag_f'44 flag_f'47 flag_f'46 flag_f'45 _23 k'97 k'98 k'103 k'99 k'102 k'103 k'104); False -> (case flag_f'44 of { True -> (call'16 flag_f'45 flag_f'47 flag_f'46 flag_f'44 _23 k'97 k'98 k'99 k'102 k'103 k'102 k'104); False -> k'101 _23 }) }) }) }) ++ (case flag_f'47 of { True -> (call'23 flag_f'47 flag_f'44 flag_f'46 flag_f'45 _22 k'97 k'104 k'98 k'99 k'102 k'103 k'103); False -> (case flag_f'44 of { True -> (call'24 flag_f'45 flag_f'47 flag_f'46 flag_f'44 _22 k'97 k'98 k'99 k'102 k'103 k'102 k'104); False -> (case flag_f'46 of { True -> (call'12 flag_f'47 flag_f'44 flag_f'46 flag_f'45 _22 k'97 k'104 k'99 k'104 k'98 k'102 k'103 k'98); False -> (case flag_f'45 of { True -> (call'25 flag_f'46 flag_f'44 flag_f'45 flag_f'47 _22 k'97 k'104 k'99 k'99 k'97 k'102 k'103 k'98); False -> k'100 _22 }) }) }) }))

call'10 = let
  k'105 = sel3 ret'2
  k'107 = sel2 ret
  k'110 = sel2 (call'28 False False True False k'108 undefined k'107 k'110 k'106 k'105 k'109)
  k'109 = sel1 (call'30 False False True False k'108 k'107 k'110 k'106 k'105 k'109 undefined)
  ret'2 = (call'31 True False False False k'108 undefined undefined k'110 k'107 k'106 k'105 k'109)
  k'108 = sel1 ret'2
  ret = (call'32 False False True False k'108 k'107 k'110 k'106 k'105 undefined k'109 undefined)
  k'106 = sel3 ret
  in (,,,,,,) k'106 k'107 k'108 k'109 (sel1 ret) k'110 k'105

call'14 flag_f'48 flag_f'49 flag_f'50 flag_f'51 s'3 k'111 k'112 k'113 k'114 k'115 k'116 k'117 = 
  let _24 = s'3 + (1::Int) in
  s'3 : (case flag_f'50 of { True -> (call'17 flag_f'48 flag_f'49 flag_f'50 flag_f'51 _24 k'111 k'112 k'113 k'117 k'112 k'114 k'115 k'117); False -> (case flag_f'49 of { True -> (call'18 flag_f'51 flag_f'48 flag_f'50 flag_f'49 _24 k'111 k'112 k'113 k'114 k'115 k'114 k'117); False -> (case flag_f'51 of { True -> (call'19 flag_f'50 flag_f'49 flag_f'51 flag_f'48 _24 k'111 k'112 k'113 k'113 k'111 k'114 k'115 k'117); False -> (case flag_f'48 of { True -> (call'20 flag_f'48 flag_f'49 flag_f'50 flag_f'51 _24 k'111 k'112 k'115 k'113 k'114 k'115 k'117); False -> k'116 _24 }) }) }) })

call'8 flag_f'52 flag_f'53 flag_f'54 flag_f'55 _25 k'118 k'119 k'120 k'121 k'122 k'123 k'124 = 
  let _26 = _25 + (1::Int) in
  _25 : (case flag_f'54 of { True -> (call'17 flag_f'53 flag_f'55 flag_f'54 flag_f'52 _26 k'118 k'119 k'120 k'124 k'119 k'121 k'122 k'124); False -> (case flag_f'55 of { True -> (call'18 flag_f'52 flag_f'53 flag_f'54 flag_f'55 _26 k'118 k'119 k'120 k'121 k'122 k'121 k'124); False -> (case flag_f'52 of { True -> (call'19 flag_f'54 flag_f'55 flag_f'52 flag_f'53 _26 k'118 k'119 k'120 k'120 k'118 k'121 k'122 k'124); False -> (case flag_f'53 of { True -> (call'20 flag_f'53 flag_f'55 flag_f'54 flag_f'52 _26 k'118 k'119 k'122 k'120 k'121 k'122 k'124); False -> k'123 _26 }) }) }) })

call'30 flag_f'56 flag_f'57 flag_f'58 flag_f'59 k'125 k'126 k'127 k'128 k'129 k'130 _27 = (,) _27 (lam'3 flag_f'57 flag_f'59 flag_f'58 flag_f'57 flag_f'58 flag_f'59 flag_f'56 flag_f'56 k'125 k'128 k'129 k'130 k'127 _27 k'126)

call'35 = 
  let t = t in
  t

foo_4 = call'29

lam'3 flag_f_k'2 flag_f_k'2' flag_f'60 flag_f'61 flag_f_k'2'2 flag_f'62 flag_f'63 flag_f_k'2'3 k'131 k'132 k'133 k'134 k'135 k'136 k'137 = \s'4 -> 
  let _28 = s'4 + (1::Int) in
  s'4 : (case flag_f_k'2'2 of { True -> (call'17 flag_f'61 flag_f'62 flag_f'60 flag_f'63 _28 k'131 k'137 k'133 k'132 k'137 k'134 k'135 k'132); False -> (case flag_f_k'2' of { True -> (call'18 flag_f'63 flag_f'61 flag_f'60 flag_f'62 _28 k'131 k'137 k'133 k'134 k'135 k'134 k'132); False -> (case flag_f_k'2'3 of { True -> (call'19 flag_f'60 flag_f'62 flag_f'63 flag_f'61 _28 k'131 k'137 k'133 k'133 k'131 k'134 k'135 k'132); False -> (case flag_f_k'2 of { True -> (call'20 flag_f'61 flag_f'62 flag_f'60 flag_f'63 _28 k'131 k'137 k'135 k'133 k'134 k'135 k'132); False -> k'136 _28 }) }) }) })

call'23 flag_f'64 flag_f'65 flag_f'66 flag_f'67 _29 k'138 k'139 k'140 k'141 k'142 k'143 k'144 = _29 : (case flag_f'66 of { True -> (call'13 flag_f'65 flag_f'67 flag_f'66 flag_f'64 _29 k'138 k'140 k'141 k'139 k'140 k'142 k'143 k'139); False -> (case flag_f'65 of { True -> (call'14 flag_f'64 flag_f'65 flag_f'66 flag_f'67 _29 k'138 k'140 k'141 k'142 k'143 k'142 k'139); False -> (case flag_f'67 of { True -> (call flag_f'66 flag_f'65 flag_f'67 flag_f'64 _29 k'138 k'140 k'141 k'141 k'138 k'142 k'143 k'139); False -> (case flag_f'64 of { True -> (call'15 flag_f'64 flag_f'65 flag_f'66 flag_f'67 _29 k'138 k'139 k'140 k'141 k'142 k'143 k'143); False -> k'144 _29 }) }) }) })

call'22 flag_f'68 flag_f'69 flag_f'70 flag_f'71 _30 k'145 k'146 k'147 k'148 k'149 k'150 k'151 = _30 : (case flag_f'70 of { True -> (call'13 flag_f'68 flag_f'71 flag_f'70 flag_f'69 _30 k'145 k'146 k'148 k'151 k'146 k'149 k'150 k'151); False -> (case flag_f'68 of { True -> (call'14 flag_f'69 flag_f'68 flag_f'70 flag_f'71 _30 k'145 k'146 k'148 k'149 k'150 k'149 k'151); False -> (case flag_f'71 of { True -> (call flag_f'70 flag_f'68 flag_f'71 flag_f'69 _30 k'145 k'146 k'148 k'148 k'145 k'149 k'150 k'151); False -> (case flag_f'69 of { True -> (call'15 flag_f'69 flag_f'68 flag_f'70 flag_f'71 _30 k'145 k'151 k'146 k'148 k'149 k'150 k'150); False -> k'147 _30 }) }) }) })

call'34 x' k'152 k'153 k'154 k'155 k'156 k'157 = x' : (case mod x' (2::Int) == (0::Int) of { True -> (call' False False True False (x' + (1::Int)) k'152 k'153 k'154 k'154 k'152 k'155 k'156 k'157); False -> (call'5 False False True False (x' * (2::Int)) k'152 k'157 k'154 k'154 k'152 k'155 k'156 k'153) })

call'3 flag_f'72 flag_f'73 flag_f'74 flag_f'75 _31 k'158 k'159 k'160 k'161 k'162 k'163 k'164 k'165 = let
  _32 = _31 + (1::Int)
  _33 = _31 * (2::Int)
  in _31 : ((case flag_f'74 of { True -> (call'11 flag_f'72 flag_f'75 flag_f'74 flag_f'73 _32 k'158 k'159 k'160 k'165 k'159 k'163 k'164 k'165); False -> (case flag_f'75 of { True -> (call'21 flag_f'72 flag_f'74 flag_f'75 flag_f'73 _32 k'158 k'159 k'160 k'160 k'158 k'163 k'164 k'165); False -> (case flag_f'73 of { True -> (call'22 flag_f'72 flag_f'73 flag_f'74 flag_f'75 _32 k'158 k'159 k'164 k'160 k'163 k'164 k'165); False -> (case flag_f'72 of { True -> (call'16 flag_f'75 flag_f'73 flag_f'74 flag_f'72 _32 k'158 k'159 k'160 k'163 k'164 k'163 k'165); False -> k'162 _32 }) }) }) }) ++ (case flag_f'73 of { True -> (call'23 flag_f'73 flag_f'72 flag_f'74 flag_f'75 _33 k'158 k'165 k'159 k'160 k'163 k'164 k'164); False -> (case flag_f'72 of { True -> (call'24 flag_f'75 flag_f'73 flag_f'74 flag_f'72 _33 k'158 k'159 k'160 k'163 k'164 k'163 k'165); False -> (case flag_f'74 of { True -> (call'12 flag_f'73 flag_f'72 flag_f'74 flag_f'75 _33 k'158 k'165 k'160 k'165 k'159 k'163 k'164 k'159); False -> (case flag_f'75 of { True -> (call'25 flag_f'74 flag_f'72 flag_f'75 flag_f'73 _33 k'158 k'165 k'160 k'160 k'158 k'163 k'164 k'159); False -> k'161 _33 }) }) }) }))

call'31 flag_f'76 flag_f'77 flag_f'78 flag_f'79 k'166 _34 _35 k'167 k'168 k'169 k'170 k'171 = (,,) _35 (lam flag_f'77 flag_f'79 flag_f'78 flag_f'77 flag_f'78 flag_f'79 flag_f'76 flag_f'76 k'166 k'169 k'170 _34 _35 k'171 k'167 k'168) _34

call'19 flag_f'80 flag_f'81 flag_f'82 flag_f'83 _36 k'172 k'173 k'174 k'175 k'176 k'177 k'178 k'179 = let
  _37 = _36 * (2::Int)
  _38 = _36 + (1::Int)
  in _36 : (case mod _36 (2::Int) == (0::Int) of { True -> (case flag_f'82 of { True -> (call' flag_f'81 flag_f'83 flag_f'82 flag_f'80 _38 k'172 k'173 k'174 k'174 k'172 k'177 k'178 k'179); False -> (case flag_f'83 of { True -> (call'2 flag_f'81 flag_f'82 flag_f'80 flag_f'83 _38 k'172 k'173 k'178 k'174 k'177 k'178 k'179); False -> (case flag_f'80 of { True -> (call'3 flag_f'81 flag_f'83 flag_f'80 flag_f'82 _38 k'172 k'173 k'174 k'179 k'173 k'177 k'178 k'179); False -> (case flag_f'81 of { True -> (call'4 flag_f'82 flag_f'83 flag_f'80 flag_f'81 _38 k'172 k'173 k'174 k'177 k'178 k'177 k'179); False -> k'176 _38 }) }) }) }); False -> (case flag_f'82 of { True -> (call'5 flag_f'80 flag_f'81 flag_f'82 flag_f'83 _37 k'172 k'179 k'174 k'174 k'172 k'177 k'178 k'173); False -> (case flag_f'83 of { True -> (call'6 flag_f'83 flag_f'81 flag_f'80 flag_f'82 _37 k'172 k'179 k'173 k'174 k'177 k'178 k'178); False -> (case flag_f'80 of { True -> (call'7 flag_f'83 flag_f'81 flag_f'80 flag_f'82 _37 k'172 k'179 k'174 k'179 k'173 k'177 k'178 k'173); False -> (case flag_f'81 of { True -> (call'8 flag_f'82 flag_f'83 flag_f'80 flag_f'81 _37 k'172 k'173 k'174 k'177 k'178 k'177 k'179); False -> k'175 _37 }) }) }) }) })

call'25 flag_f'84 flag_f'85 flag_f'86 flag_f'87 _39 k'180 k'181 k'182 k'183 k'184 k'185 k'186 k'187 = let
  _40 = _39 * (2::Int)
  _41 = _39 + (1::Int)
  in _39 : (case mod _39 (2::Int) == (0::Int) of { True -> (case flag_f'86 of { True -> (call' flag_f'85 flag_f'87 flag_f'86 flag_f'84 _41 k'180 k'187 k'182 k'182 k'180 k'185 k'186 k'181); False -> (case flag_f'87 of { True -> (call'2 flag_f'85 flag_f'86 flag_f'84 flag_f'87 _41 k'180 k'187 k'186 k'182 k'185 k'186 k'181); False -> (case flag_f'84 of { True -> (call'3 flag_f'85 flag_f'87 flag_f'84 flag_f'86 _41 k'180 k'187 k'182 k'181 k'187 k'185 k'186 k'181); False -> (case flag_f'85 of { True -> (call'4 flag_f'86 flag_f'87 flag_f'84 flag_f'85 _41 k'180 k'187 k'182 k'185 k'186 k'185 k'181); False -> k'184 _41 }) }) }) }); False -> (case flag_f'86 of { True -> (call'5 flag_f'84 flag_f'85 flag_f'86 flag_f'87 _40 k'180 k'181 k'182 k'182 k'180 k'185 k'186 k'187); False -> (case flag_f'87 of { True -> (call'6 flag_f'87 flag_f'85 flag_f'84 flag_f'86 _40 k'180 k'181 k'187 k'182 k'185 k'186 k'186); False -> (case flag_f'84 of { True -> (call'7 flag_f'87 flag_f'85 flag_f'84 flag_f'86 _40 k'180 k'181 k'182 k'181 k'187 k'185 k'186 k'187); False -> (case flag_f'85 of { True -> (call'8 flag_f'86 flag_f'87 flag_f'84 flag_f'85 _40 k'180 k'187 k'182 k'185 k'186 k'185 k'181); False -> k'183 _40 }) }) }) }) })

lam'4 = \f -> 
  let _42 = f _42 in
  _42

foo_2 = call'35

call'20 flag_f'88 flag_f'89 flag_f'90 flag_f'91 _43 k'188 k'189 k'190 k'191 k'192 k'193 k'194 = _43 : (case flag_f'90 of { True -> (call'13 flag_f'89 flag_f'91 flag_f'90 flag_f'88 _43 k'188 k'189 k'191 k'194 k'189 k'192 k'193 k'194); False -> (case flag_f'89 of { True -> (call'14 flag_f'88 flag_f'89 flag_f'90 flag_f'91 _43 k'188 k'189 k'191 k'192 k'193 k'192 k'194); False -> (case flag_f'91 of { True -> (call flag_f'90 flag_f'89 flag_f'91 flag_f'88 _43 k'188 k'189 k'191 k'191 k'188 k'192 k'193 k'194); False -> (case flag_f'88 of { True -> (call'15 flag_f'88 flag_f'89 flag_f'90 flag_f'91 _43 k'188 k'194 k'189 k'191 k'192 k'193 k'193); False -> k'190 _43 }) }) }) })

call'12 flag_f'92 flag_f'93 flag_f'94 flag_f'95 _44 k'195 k'196 k'197 k'198 k'199 k'200 k'201 k'202 = let
  _45 = _44 + (1::Int)
  _46 = _44 * (2::Int)
  in _44 : ((case flag_f'94 of { True -> (call'11 flag_f'93 flag_f'95 flag_f'94 flag_f'92 _45 k'195 k'202 k'197 k'196 k'202 k'200 k'201 k'196); False -> (case flag_f'95 of { True -> (call'21 flag_f'93 flag_f'94 flag_f'95 flag_f'92 _45 k'195 k'202 k'197 k'197 k'195 k'200 k'201 k'196); False -> (case flag_f'92 of { True -> (call'22 flag_f'93 flag_f'92 flag_f'94 flag_f'95 _45 k'195 k'202 k'201 k'197 k'200 k'201 k'196); False -> (case flag_f'93 of { True -> (call'16 flag_f'95 flag_f'92 flag_f'94 flag_f'93 _45 k'195 k'202 k'197 k'200 k'201 k'200 k'196); False -> k'199 _45 }) }) }) }) ++ (case flag_f'92 of { True -> (call'23 flag_f'92 flag_f'93 flag_f'94 flag_f'95 _46 k'195 k'196 k'202 k'197 k'200 k'201 k'201); False -> (case flag_f'93 of { True -> (call'24 flag_f'95 flag_f'92 flag_f'94 flag_f'93 _46 k'195 k'202 k'197 k'200 k'201 k'200 k'196); False -> (case flag_f'94 of { True -> (call'12 flag_f'92 flag_f'93 flag_f'94 flag_f'95 _46 k'195 k'196 k'197 k'196 k'202 k'200 k'201 k'202); False -> (case flag_f'95 of { True -> (call'25 flag_f'94 flag_f'93 flag_f'95 flag_f'92 _46 k'195 k'196 k'197 k'197 k'195 k'200 k'201 k'202); False -> k'198 _46 }) }) }) }))

call'27 = 
  let _47 = id _47 in
  _47

call'18 flag_f'96 flag_f'97 flag_f'98 flag_f'99 _48 k'203 k'204 k'205 k'206 k'207 k'208 k'209 = 
  let _49 = _48 + (1::Int) in
  _48 : (case flag_f'98 of { True -> (call'17 flag_f'97 flag_f'99 flag_f'98 flag_f'96 _49 k'203 k'204 k'205 k'209 k'204 k'206 k'207 k'209); False -> (case flag_f'99 of { True -> (call'18 flag_f'96 flag_f'97 flag_f'98 flag_f'99 _49 k'203 k'204 k'205 k'206 k'207 k'206 k'209); False -> (case flag_f'96 of { True -> (call'19 flag_f'98 flag_f'99 flag_f'96 flag_f'97 _49 k'203 k'204 k'205 k'205 k'203 k'206 k'207 k'209); False -> (case flag_f'97 of { True -> (call'20 flag_f'97 flag_f'99 flag_f'98 flag_f'96 _49 k'203 k'204 k'207 k'205 k'206 k'207 k'209); False -> k'208 _49 }) }) }) })

call'13 flag_f'100 flag_f'101 flag_f'102 flag_f'103 s'5 k'210 k'211 k'212 k'213 k'214 k'215 k'216 k'217 = let
  _50 = s'5 * (2::Int)
  _51 = s'5 + (1::Int)
  in s'5 : ((case flag_f'102 of { True -> (call'11 flag_f'100 flag_f'101 flag_f'102 flag_f'103 _51 k'210 k'211 k'212 k'217 k'211 k'215 k'216 k'217); False -> (case flag_f'101 of { True -> (call'21 flag_f'100 flag_f'102 flag_f'101 flag_f'103 _51 k'210 k'211 k'212 k'212 k'210 k'215 k'216 k'217); False -> (case flag_f'103 of { True -> (call'22 flag_f'100 flag_f'103 flag_f'102 flag_f'101 _51 k'210 k'211 k'216 k'212 k'215 k'216 k'217); False -> (case flag_f'100 of { True -> (call'16 flag_f'101 flag_f'103 flag_f'102 flag_f'100 _51 k'210 k'211 k'212 k'215 k'216 k'215 k'217); False -> k'214 _51 }) }) }) }) ++ (case flag_f'103 of { True -> (call'23 flag_f'103 flag_f'100 flag_f'102 flag_f'101 _50 k'210 k'217 k'211 k'212 k'215 k'216 k'216); False -> (case flag_f'100 of { True -> (call'24 flag_f'101 flag_f'103 flag_f'102 flag_f'100 _50 k'210 k'211 k'212 k'215 k'216 k'215 k'217); False -> (case flag_f'102 of { True -> (call'12 flag_f'103 flag_f'100 flag_f'102 flag_f'101 _50 k'210 k'217 k'212 k'217 k'211 k'215 k'216 k'211); False -> (case flag_f'101 of { True -> (call'25 flag_f'102 flag_f'100 flag_f'101 flag_f'103 _50 k'210 k'217 k'212 k'212 k'210 k'215 k'216 k'211); False -> k'213 _50 }) }) }) }))

foo = lam'4

call'5 flag_f'104 flag_f'105 flag_f'106 flag_f'107 _52 k'218 k'219 k'220 k'221 k'222 k'223 k'224 k'225 = let
  _53 = _52 * (2::Int)
  _54 = _52 + (1::Int)
  in _52 : (case mod _52 (2::Int) == (0::Int) of { True -> (case flag_f'106 of { True -> (call' flag_f'105 flag_f'107 flag_f'106 flag_f'104 _54 k'218 k'225 k'220 k'220 k'218 k'223 k'224 k'219); False -> (case flag_f'107 of { True -> (call'2 flag_f'105 flag_f'106 flag_f'104 flag_f'107 _54 k'218 k'225 k'224 k'220 k'223 k'224 k'219); False -> (case flag_f'104 of { True -> (call'3 flag_f'105 flag_f'107 flag_f'104 flag_f'106 _54 k'218 k'225 k'220 k'219 k'225 k'223 k'224 k'219); False -> (case flag_f'105 of { True -> (call'4 flag_f'106 flag_f'107 flag_f'104 flag_f'105 _54 k'218 k'225 k'220 k'223 k'224 k'223 k'219); False -> k'222 _54 }) }) }) }); False -> (case flag_f'106 of { True -> (call'5 flag_f'104 flag_f'105 flag_f'106 flag_f'107 _53 k'218 k'219 k'220 k'220 k'218 k'223 k'224 k'225); False -> (case flag_f'107 of { True -> (call'6 flag_f'107 flag_f'105 flag_f'104 flag_f'106 _53 k'218 k'219 k'225 k'220 k'223 k'224 k'224); False -> (case flag_f'104 of { True -> (call'7 flag_f'107 flag_f'105 flag_f'104 flag_f'106 _53 k'218 k'219 k'220 k'219 k'225 k'223 k'224 k'225); False -> (case flag_f'105 of { True -> (call'8 flag_f'106 flag_f'107 flag_f'104 flag_f'105 _53 k'218 k'225 k'220 k'223 k'224 k'223 k'219); False -> k'221 _53 }) }) }) }) })

call'32 flag_f'108 flag_f'109 flag_f'110 flag_f'111 k'226 k'227 k'228 k'229 k'230 _55 k'231 _56 = (,,) (lam'5 flag_f'109 flag_f'111 flag_f'110 flag_f'109 flag_f'110 flag_f'111 flag_f'108 flag_f'108 k'226 k'229 k'230 _55 _56 k'231 k'228 k'227) _56 _55

call'7 flag_f'112 flag_f'113 flag_f'114 flag_f'115 _57 k'232 k'233 k'234 k'235 k'236 k'237 k'238 k'239 = let
  _58 = _57 * (2::Int)
  _59 = _57 + (1::Int)
  in _57 : ((case flag_f'114 of { True -> (call'11 flag_f'113 flag_f'115 flag_f'114 flag_f'112 _59 k'232 k'239 k'234 k'233 k'239 k'237 k'238 k'233); False -> (case flag_f'115 of { True -> (call'21 flag_f'113 flag_f'114 flag_f'115 flag_f'112 _59 k'232 k'239 k'234 k'234 k'232 k'237 k'238 k'233); False -> (case flag_f'112 of { True -> (call'22 flag_f'113 flag_f'112 flag_f'114 flag_f'115 _59 k'232 k'239 k'238 k'234 k'237 k'238 k'233); False -> (case flag_f'113 of { True -> (call'16 flag_f'115 flag_f'112 flag_f'114 flag_f'113 _59 k'232 k'239 k'234 k'237 k'238 k'237 k'233); False -> k'236 _59 }) }) }) }) ++ (case flag_f'112 of { True -> (call'23 flag_f'112 flag_f'113 flag_f'114 flag_f'115 _58 k'232 k'233 k'239 k'234 k'237 k'238 k'238); False -> (case flag_f'113 of { True -> (call'24 flag_f'115 flag_f'112 flag_f'114 flag_f'113 _58 k'232 k'239 k'234 k'237 k'238 k'237 k'233); False -> (case flag_f'114 of { True -> (call'12 flag_f'112 flag_f'113 flag_f'114 flag_f'115 _58 k'232 k'233 k'234 k'233 k'239 k'237 k'238 k'239); False -> (case flag_f'115 of { True -> (call'25 flag_f'114 flag_f'113 flag_f'115 flag_f'112 _58 k'232 k'233 k'234 k'234 k'232 k'237 k'238 k'239); False -> k'235 _58 }) }) }) }))

foo_6_5 = lam'2

foo_5 = sel5 call'10

lam'5 flag_f_k' flag_f_k'' flag_f'116 flag_f'117 flag_f_k''2 flag_f'118 flag_f'119 flag_f_k''3 k'240 k'241 k'242 k'243 k'244 k'245 k'246 k'247 = \s'6 -> let
  _61 = s'6 * (2::Int)
  _60 = s'6 + (1::Int)
  in s'6 : ((case flag_f_k''2 of { True -> (call'11 flag_f'117 flag_f'118 flag_f'116 flag_f'119 _60 k'240 k'247 k'242 k'241 k'247 k'245 k'246 k'241); False -> (case flag_f_k'' of { True -> (call'21 flag_f'117 flag_f'116 flag_f'118 flag_f'119 _60 k'240 k'247 k'242 k'242 k'240 k'245 k'246 k'241); False -> (case flag_f_k''3 of { True -> (call'22 flag_f'117 flag_f'119 flag_f'116 flag_f'118 _60 k'240 k'247 k'246 k'242 k'245 k'246 k'241); False -> (case flag_f_k' of { True -> (call'16 flag_f'118 flag_f'119 flag_f'116 flag_f'117 _60 k'240 k'247 k'242 k'245 k'246 k'245 k'241); False -> k'244 _60 }) }) }) }) ++ (case flag_f_k''3 of { True -> (call'23 flag_f'119 flag_f'117 flag_f'116 flag_f'118 _61 k'240 k'241 k'247 k'242 k'245 k'246 k'246); False -> (case flag_f_k' of { True -> (call'24 flag_f'118 flag_f'119 flag_f'116 flag_f'117 _61 k'240 k'247 k'242 k'245 k'246 k'245 k'241); False -> (case flag_f_k''2 of { True -> (call'12 flag_f'119 flag_f'117 flag_f'116 flag_f'118 _61 k'240 k'241 k'242 k'241 k'247 k'245 k'246 k'247); False -> (case flag_f_k'' of { True -> (call'25 flag_f'116 flag_f'117 flag_f'118 flag_f'119 _61 k'240 k'241 k'242 k'242 k'240 k'245 k'246 k'247); False -> k'243 _61 }) }) }) }))

call'2 flag_f'120 flag_f'121 flag_f'122 flag_f'123 _62 k'248 k'249 k'250 k'251 k'252 k'253 k'254 = _62 : (case flag_f'122 of { True -> (call'13 flag_f'120 flag_f'121 flag_f'122 flag_f'123 _62 k'248 k'249 k'251 k'254 k'249 k'252 k'253 k'254); False -> (case flag_f'120 of { True -> (call'14 flag_f'123 flag_f'120 flag_f'122 flag_f'121 _62 k'248 k'249 k'251 k'252 k'253 k'252 k'254); False -> (case flag_f'121 of { True -> (call flag_f'122 flag_f'120 flag_f'121 flag_f'123 _62 k'248 k'249 k'251 k'251 k'248 k'252 k'253 k'254); False -> (case flag_f'123 of { True -> (call'15 flag_f'123 flag_f'120 flag_f'122 flag_f'121 _62 k'248 k'254 k'249 k'251 k'252 k'253 k'253); False -> k'250 _62 }) }) }) })

call'15 flag_f'124 flag_f'125 flag_f'126 flag_f'127 s'7 k'255 k'256 k'257 k'258 k'259 k'260 k'261 = s'7 : (case flag_f'126 of { True -> (call'13 flag_f'125 flag_f'127 flag_f'126 flag_f'124 s'7 k'255 k'257 k'258 k'256 k'257 k'259 k'260 k'256); False -> (case flag_f'125 of { True -> (call'14 flag_f'124 flag_f'125 flag_f'126 flag_f'127 s'7 k'255 k'257 k'258 k'259 k'260 k'259 k'256); False -> (case flag_f'127 of { True -> (call flag_f'126 flag_f'125 flag_f'127 flag_f'124 s'7 k'255 k'257 k'258 k'258 k'255 k'259 k'260 k'256); False -> (case flag_f'124 of { True -> (call'15 flag_f'124 flag_f'125 flag_f'126 flag_f'127 s'7 k'255 k'256 k'257 k'258 k'259 k'260 k'260); False -> k'261 s'7 }) }) }) })

call'26 = let
  k'262 = sel3 ret'2
  k'265 = sel2 ret'3
  k'266 = sel3 ret
  ret = (call'32 True False False False k'263 k'264 k'265 k'266 k'262 undefined k'267 undefined)
  k'264 = sel2 ret
  ret'2 = (call'31 False False False True k'263 undefined undefined k'265 k'264 k'266 k'262 k'267)
  k'263 = sel1 ret'2
  k'267 = sel1 (call'30 False True False False k'263 k'264 k'265 k'266 k'262 k'267 undefined)
  ret'3 = (call'28 True False False False k'263 undefined k'264 k'265 k'266 k'262 k'267)
  λ' = sel1 ret'3
  in λ'

call'33 = let
  k'268 = sel3 ret'2
  k'271 = sel1 ret'2
  k'269 = sel3 ret
  k'273 = sel2 (call'28 False False False True k'271 _63 k'270 k'273 k'269 k'268 k'272)
  k'272 = sel1 (call'30 True False False False k'271 k'270 k'273 k'269 k'268 k'272 _63)
  ret'2 = (call'31 False False True False k'271 _63 _63 k'273 k'270 k'269 k'268 k'272)
  _63 = undefined
  ret = (call'32 False False False True k'271 k'270 k'273 k'269 k'268 _63 k'272 _63)
  k'270 = sel2 ret
  in (,,,,,,) k'269 k'270 (sel2 ret'2) k'271 k'272 k'273 k'268
