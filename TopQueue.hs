module TopQueue where

import Base
import Core
import Pack
import Queue

serve :: CoreOut -> Queue 64 Pack -> (CoreIn, Queue 64 Pack)
serve co q = (CoreIn { nextPack=np' (ready co)
              , dfuInstr=Nothing
              , dfuData=Nothing
              , cfuInstr=Nothing
              }, q' (packOut co) (packType co) (ready co))
    where
    np' True = Just (top q)
    np' _  = Nothing
    q' _ _ True = pop q
    q' p None _ = q
    q' p Core.Queue _ = push p q
    --TODO(bjorn): Add memory to write frame to.
    q' p Frame _ = undefined 
