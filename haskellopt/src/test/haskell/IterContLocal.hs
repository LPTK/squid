-- Like IterCont, but with a local definition of `loop`
-- module IterCont where
module Main where

import System.Exit

-- main = exitSuccess
main =
    let n3 = take 5 nats in
    if n3 == [0,1,2,3,4]
    then exitSuccess else do { print n3; exitFailure }

-- FIXME generates inf (+1) loop
-- nats =
--     loop (\k s -> s : k (s + 1)) 0 where
--         loop f state =
--             f (\new_state -> loop f new_state) state

-- Simpler version, to debug:
nats =
    -- loop (\k s -> s : k s) 0 where
    loop (\k s -> s : k (s + 1)) 0 where
    loop f state =
        rec state where
        rec st = f (\new_st -> rec new_st) st

-- FIXME SOF
-- (count, nats) =
--     ( loop (\k s -> k s) 0
--     -- , loop (\k' s' -> k' s') 0
--     , loop (\k' -> id) 0 -- FIXME SOF
--     ) where
--         loop f state =
--             f (\new_state -> loop f new_state) state
