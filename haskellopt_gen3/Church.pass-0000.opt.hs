-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   desugar
-- Beta reductions:  69
-- Incl. one-shot:   0
-- Case reductions:  0
-- Field reductions: 0
-- Case commutings:  0
-- Total nodes: 2060; Boxes: 887; Branches: 880
-- Apps: 109; Lams: 57

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Church (zero_x_three,two_x_zero,two_x_three,mult,two_p_three,plus,three,three_id,two_id,two,one_id,one,zero,_I) where

import Data.Tuple.Select

lam s'2 = \z' -> s'2 (s'2 (s'2 z'))

lam' = \s'3 -> lam'2

lam'3 flag_s' s'4 = \z'2 -> case flag_s' of { True -> z'2; False -> s'4 z'2 }

call = (call' False True False lam' undefined (sel1 (call'2 False True False undefined)))

lam'4 flag_m_n' flag_m_n'' flag_n_s flag_n_z_s flag_n_z_s' flag_n_z flag_n_s' flag_n_z_s'2 flag_n' flag_z flag_n_s'2 flag_n flag_n_z_s'3 flag_m flag_n_z' flag_n_z_s'4 flag_n'' flag_z' flag_n''2 flag_n_z_s'5 flag_n_z_s'6 flag_n_z'2 flag_n_z_s'7 flag_n_s'3 flag_n_z'3 flag_m_n''2 s'5 n'2 m'2 n'3 m'3 n'4 = \z'3 -> let
  _0 = case flag_n_z_s'3 of { True -> z'3; False -> (case flag_n_z' of { True -> z'3; False -> (case flag_n of { True -> (call'6 z'3 s'5); False -> (case flag_n_z_s'4 of { True -> z'3; False -> (case flag_n_s'2 of { True -> (call'7 flag_n''2 flag_z' flag_n'' flag_z flag_n' z'3 s'5 m'2 n'3); False -> (case flag_n_z_s'2 of { True -> z'3; False -> (case flag_n_z_s'5 of { True -> z'3; False -> (case flag_n_s' of { True -> (call'8 flag_n''2 flag_z' flag_n'' flag_z flag_n' z'3 s'5 m'2 n'3); False -> (case flag_n_z_s'6 of { True -> z'3; False -> (case flag_n_z of { True -> z'3; False -> (case flag_n_z'2 of { True -> z'3; False -> (case flag_n_z_s' of { True -> z'3; False -> (case flag_n_z_s'7 of { True -> z'3; False -> (case flag_n_z_s of { True -> z'3; False -> (case flag_n_s'3 of { True -> (call'9 flag_n''2 flag_z' flag_n'' flag_z flag_n' z'3 s'5 m'2 n'2); False -> (case flag_n_s of { True -> (call'10 flag_n''2 flag_z' flag_n'' flag_z flag_n' z'3 s'5 m'2 n'2); False -> (case flag_n_z'3 of { True -> z'3; False -> n'4 s'5 z'3 }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }
  ret = (call'5 s'5)
  in case flag_m of { True -> sel1 ret (sel2 ret _0); False -> (case flag_m_n'' of { True -> (call'3 _0 s'5); False -> (case flag_m_n''2 of { True -> _0; False -> (case flag_m_n' of { True -> (call'4 _0 s'5); False -> m'3 s'5 _0 }) }) }) }

two_id = call'11

call'8 flag_n''3 flag_z'2 flag_n''4 flag_z'3 flag_n''5 z'4 s'6 m'4 n'5 = 
  let _1 = case flag_z'3 of { True -> z'4; False -> (case flag_z'2 of { True -> z'4; False -> n'5 s'6 z'4 }) } in
  case flag_n''4 of { True -> (call'3 _1 s'6); False -> (case flag_n''5 of { True -> _1; False -> (case flag_n''3 of { True -> (call'4 _1 s'6); False -> m'4 s'6 _1 }) }) }

call'12 s'7 = s'7

call'13 _2 s'8 s'9 = s'9 (s'8 _2)

one = lam'5

lam'6 flag_n'_m flag_n'_m' flag_n'_m'2 flag_m' m'5 = \n -> (lam'7 flag_n'_m'2 flag_n'_m' False False False False False False False False False False False flag_m' False False False False False False False False False False False flag_n'_m n undefined undefined undefined m'5)

two_x_three = call

plus = lam'8

call'14 s'10 = s'10

call'4 _3 s'11 = s'11 (s'11 (s'11 _3))

call'15 s'12 = s'12

call'16 flag_z'4 flag_s_z flag_z'5 flag_s_z' flag_s flag_n''6 flag_n''7 flag_s_z'2 flag_n''8 flag_s' flag_s_z'3 _4 n'6 m'6 m'7 = (lam'7 flag_n''6 flag_n''7 flag_s flag_s_z'2 flag_s_z' False False False flag_n''8 flag_z'5 flag_s' False flag_s_z False False flag_s_z'3 flag_n''7 flag_z'4 flag_n''6 False False False False False False flag_n''8 _4 m'6 undefined n'6 m'7)

call'2 flag_n''9 flag_n''10 flag_n''11 n'7 = (,) n'7 (lam'6 flag_n''9 flag_n''11 flag_n''10 False n'7)

call'17 = undefined

call'18 = undefined

lam'9 = \s'13 -> (lam s'13)

_I = lam'10

two_p_three = (call'19 undefined)

lam'11 = \s -> (lam'12 False False False False False False False s s undefined)

lam'7 flag_m_n''3 flag_m_n''4 flag_s_n flag_z_s_n flag_z_s_n' flag_z_n flag_s_n' flag_z_s_n'2 flag_n''12 flag_z'6 flag_s_n'2 flag_n' flag_z_s_n'3 flag_m'2 flag_z_n' flag_z_s_n'4 flag_n''13 flag_z'7 flag_n''14 flag_z_s_n'5 flag_z_s_n'6 flag_z_n'2 flag_z_s_n'7 flag_s_n'3 flag_z_n'3 flag_m_n''5 n'8 m'8 n'9 n'10 m'9 = \s'14 -> (lam'4 flag_m_n''3 flag_m_n''4 flag_s_n flag_z_s_n flag_z_s_n' flag_z_n flag_s_n' flag_z_s_n'2 flag_n''12 flag_z'6 flag_s_n'2 flag_n' flag_z_s_n'3 flag_m'2 flag_z_n' flag_z_s_n'4 flag_n''13 flag_z'7 flag_n''14 flag_z_s_n'5 flag_z_s_n'6 flag_z_n'2 flag_z_s_n'7 flag_s_n'3 flag_z_n'3 flag_m_n''5 s'14 n'9 m'8 n'10 m'9 n'8)

call'20 s'15 = s'15

call'10 flag_n''15 flag_z'8 flag_n''16 flag_z'9 flag_n''17 z'5 s'16 m'10 n'11 = 
  let _5 = case flag_z'9 of { True -> z'5; False -> (case flag_z'8 of { True -> z'5; False -> n'11 s'16 z'5 }) } in
  case flag_n''16 of { True -> (call'3 _5 s'16); False -> (case flag_n''17 of { True -> _5; False -> (case flag_n''15 of { True -> (call'4 _5 s'16); False -> m'10 s'16 _5 }) }) }

call'21 flag_n''18 flag_n''19 flag_n''20 λ s'17 m'11 = (call'22 flag_n''18 flag_n''19 False False False False False True flag_n''20 (s'17 λ) (sel2 (call'23 flag_n''20 False flag_n''18 flag_n''19 True undefined m'11)) m'11)

call'24 = (call'21 False False True lam' undefined (sel1 (call'2 True False False undefined)))

three_id = call'25

call'26 s'18 = s'18

lam'10 = \x -> x

call'27 flag_n''21 flag_z'10 flag_n''22 flag_n''23 flag_z'11 z'6 m'12 = (,) (lam'7 flag_n''22 flag_n''23 False False False flag_z'10 False False False False False False False False flag_z'11 False False False False False False False False False False flag_n''21 z'6 undefined undefined undefined m'12) z'6

one_id = call'28

lam'13 flag_m' flag_m'' flag_m''2 m'13 = \n' -> let
  ret' = (call'2 False False False n')
  m'14 = sel1 ret'
  in case flag_m'' of { True -> lam'; False -> (case flag_m''2 of { True -> (call' False False False lam' undefined m'14); False -> (case flag_m' of { True -> (call'21 False False False lam' undefined m'14); False -> m'13 (sel2 ret') lam' }) }) }

call'6 z'7 s'19 = s'19 (s'19 (s'19 z'7))

two_x_zero = call'24

call' flag_n''24 flag_n''25 flag_n''26 λ' s'20 m'15 = (call'16 False False True False False flag_n''25 flag_n''26 False flag_n''24 True False (s'20 λ') (sel2 (call'27 flag_n''24 False flag_n''25 flag_n''26 True undefined m'15)) m'15 m'15)

call'29 s'21 = s'21

call'28 = (lam'3 True undefined)

call'3 _6 s'22 = s'22 (s'22 (s'22 _6))

lam'14 = \m' -> (lam'13 False False False m')

lam'5 = \s' -> (lam'3 False s')

zero_x_three = lam'

two = lam'11

lam'8 = \m -> (lam'6 False False False False m)

call'7 flag_n''27 flag_z'12 flag_n''28 flag_z'13 flag_n''29 z'8 s'23 m'16 n'12 = 
  let _7 = case flag_z'13 of { True -> z'8; False -> (case flag_z'12 of { True -> z'8; False -> n'12 s'23 z'8 }) } in
  case flag_n''28 of { True -> (call'3 _7 s'23); False -> (case flag_n''29 of { True -> _7; False -> (case flag_n''27 of { True -> (call'4 _7 s'23); False -> m'16 s'23 _7 }) }) }

mult = lam'14

zero = lam'

call'22 flag_n''30 flag_n''31 flag_s_z'4 flag_s'2 flag_s_z'5 flag_s_z'6 flag_s_z'7 flag_s'3 flag_n''32 _8 n'13 m'17 = (lam'7 flag_n''30 flag_n''31 False False False False flag_s'2 flag_s_z'5 False False False False False False False False False False False flag_s_z'6 flag_s_z'7 False flag_s_z'4 flag_s'3 False flag_n''32 _8 undefined n'13 undefined m'17)

call'23 flag_n''33 flag_z'14 flag_n''34 flag_n''35 flag_z'15 z'9 m'18 = (,) (lam'7 flag_n''34 flag_n''35 False False False False False False False False False False False False False False False False False False False flag_z'14 False False flag_z'15 flag_n''33 z'9 undefined undefined undefined m'18) z'9

call'11 = (lam'12 False False False False True False False undefined undefined undefined)

call'5 s'24 = (,) s'24 s'24

call'30 = undefined

lam'2 = \z'10 -> z'10

call'31 = lam'

lam'12 flag_s'4 flag_n''36 flag_s'5 flag_s'6 flag_s'7 flag_n''37 flag_n''38 s'25 s'26 m'19 = \z -> let
  _10 = s'25 z
  ret'2 = (call'27 flag_n''38 False flag_n''37 flag_n''36 False z m'19)
  ret'3 = (call'23 flag_n''38 False flag_n''37 flag_n''36 False z m'19)
  _9 = case flag_s'6 of { True -> z; False -> (case flag_s'7 of { True -> z; False -> (case flag_s'5 of { True -> sel1 ret'2; False -> (case flag_s'4 of { True -> sel1 ret'3; False -> _10 }) }) }) }
  in case flag_s'6 of { True -> _9; False -> (case flag_s'7 of { True -> _9; False -> (case flag_s'5 of { True -> (call'16 False undefined False undefined flag_s'4 flag_n''37 flag_n''36 undefined flag_n''38 flag_s'5 undefined (case flag_s'7 of { True -> z; False -> _10 }) (sel2 ret'2) m'19 m'19); False -> (case flag_s'4 of { True -> (call'22 flag_n''37 flag_n''36 undefined flag_s'5 undefined undefined undefined flag_s'4 flag_n''38 (case flag_s'7 of { True -> z; False -> _10 }) (sel2 ret'3) m'19); False -> s'26 _9 }) }) }) }

three = lam'9

call'32 s'27 = s'27

call'9 flag_n''39 flag_z'16 flag_n''40 flag_z'17 flag_n''41 z'11 s'28 m'20 n'14 = 
  let _11 = case flag_z'17 of { True -> z'11; False -> (case flag_z'16 of { True -> z'11; False -> n'14 s'28 z'11 }) } in
  case flag_n''40 of { True -> (call'3 _11 s'28); False -> (case flag_n''41 of { True -> _11; False -> (case flag_n''39 of { True -> (call'4 _11 s'28); False -> m'20 s'28 _11 }) }) }

call'25 = (lam'12 False False False True False False False undefined undefined undefined)

call'19 m'21 = (lam'7 False False False False False False False False False False False True False True False False False False False False False False False False False False undefined undefined undefined undefined m'21)
