module TopQueue where

import Core
import Queue

initTopQueue = undefined

serve :: CoreOut -> Queue -> (CoreIn, Queue)
serve CO Q = ({ nextPack=np' ()()()
              , dfuInstr=0
              , dfuData=0
              , cfuInstr=0
              }, Q')
    where
