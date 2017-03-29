module TopQueue where

import Base
import Core
import Pack
import Queue

width = d16
height = d16 
frameBuffer = asyncRam (width `mulSNat` height)

type QueueState = (Queue 64 Pack, Signal Value)

serve :: CoreOut -> QueueState -> (CoreIn, QueueState)
serve co fb (q, s) = ( CoreIn { nextPack=np' (ready co)
                           , dfuInstr=Nothing
                           , dfuData=Nothing
                           , cfuInstr=Nothing
                           }
                   , ( q' (packOut co) (packType co) (ready co)
                     , s' (packOut co) (packType co) 
                     )
                   )
    where
    np' True = Just (top q)
    np' _  = Nothing
    q' _ _ True = pop q
    q' p Core.Queue _ = push p q
    q' _ _ _ = q
    s' (col, pos) Frame = frameBuffer (Signal addr) (Signal (Just (addr, color))) 
    s' (col, pos) _ = frameBuffer (Signal addr) (Signal Nothing) 
        where
        x = (pos !! 0)
        y = (pos !! 1)
        addr = ((natVal width) * y + x)
        color = 





