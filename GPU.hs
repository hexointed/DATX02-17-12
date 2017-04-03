module GPU where

import Base
import Core hiding (pack)
import Pack
import Queue
import TopQueue
import Memory


frameBufferWidth = d16
frameBufferHeight = d16
frameBuffer = asyncRam (frameBufferWidth `mulSNat` frameBufferHeight)

gpu = sigPxOut
        where
        sigPxOut = frameBuffer sigAddr sigWritePx
        (sigCoreIn, sigAddr, sigWritePx) = unbundle $ mealyCombi sigCoreOut
        sigCoreOut = mealyStep sigCoreIn

mealyStep = mealy step' initial'

mealyCombi = mealy combi (empty :: TopQueue)

combi queue coreOut = (newQueue, ((CoreIn pack dfui dfud cfui), addr, px))  
        where
        ((CoreIn pack _ _ _), newQueue, addr, px) = serve coreOut queue
        (CoreIn _ dfui dfud cfui) = fetch coreOut
