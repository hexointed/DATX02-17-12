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

gpu = pixelOut
        where
        pixelOut = frameBuffer (signal addr) (signal px)
        coreIn = CoreIn { nextPack = pack
                        , dfuInstr = dfui
                        , dfuData = dfud
                        , cfuInstr = cfui
                        }
        ((CoreIn pack _ _ _), newQueue, addr, px) = serve coreOut queue
        (CoreIn _ dfui dfud cfui) = fetch coreOut
        queue = newQueue
        (core, coreOut) = step' initial' coreIn
