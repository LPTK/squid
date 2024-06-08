-- Like IterCont, but with a local definition of `loop`
-- module IterCont where
module IterContLocal where

nats0 =
    loop (\k s -> s : k (s + 1)) 0 where
        loop f state =
            -- f (\new_state -> loop f new_state) state
            f (loop f) state

nats1 =
    loop (\k s -> s : k (s + 1)) 0 where
    loop f state =
        rec state where
        -- rec st = f (\new_st -> rec new_st) st
        rec st = f rec st

nats1_5 = take 5 nats1

-- FIXME graph-loader problem for this:
-- (count, nats) =
--     ( loop (\k s -> k s) 0
--     -- , loop (\k' s' -> k' s') 0
--     , loop (\k' -> id) 0 -- FIXME SOF
--     ) where
--         loop f state =
--             f (\new_state -> loop f new_state) state