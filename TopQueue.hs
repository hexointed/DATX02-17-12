{-# LANGUAGE MagicHash #-}

module TopQueue where

import Base
import Core
import Pack
import Queue
import CLaSH.Sized.Fixed
import CLaSH.Sized.Internal.Signed
import CLaSH.Sized.BitVector

width = d16
height = d16

frameBuffer = asyncRam (width `mulSNat` height)

serve :: CoreOut -> Queue 64 Pack -> (CoreIn, Queue 64 Pack, Signal (BitVector d24))
serve ( CoreOut { packOut  = p
                , packType = pt
                , ready    = rdy
                } 
      ) q = ( CoreIn { nextPack  = np' rdy
                      , dfuInstr = Nothing
                      , dfuData  = Nothing
                      , cfuInstr = Nothing
                      }
            , q' pt rdy
            , s' pt
            )
  where
    np' True        = Just (top q)
    np' _           = Nothing
    q' _ True       = pop q
    q' Core.Queue _ = push p q
    q' _ _          = q
    s' Frame        = frameBuffer (signal addr) (signal (Just (addr, color))) 
    s' _            = frameBuffer (signal addr) (signal Nothing) 
    color           = r ++# g ++# b
    r               = bSlice d32 d40 $ pack# $ unSF (p !! 2)
    g               = bSlice d32 d40 $ pack# $ unSF (p !! 3)
    b               = bSlice d32 d40 $ pack# $ unSF (p !! 4)
    addr            = (natVal width) * y + x
    x               = toInteger# $ unpack# $ bSlice d32 d64 $ pack# $ unSF (p !! 0)
    y               = toInteger# $ unpack# $ bSlice d32 d64 $ pack# $ unSF (p !! 1)
