module TopQueue where

import Base
import Core hiding (pack)
import Pack
import Queue
import CLaSH.Sized.Fixed
import CLaSH.Class.BitPack
import CLaSH.Sized.BitVector

type TopQueue = Queue 64 Pack
type Address = Signed 32
type RGBValue = BitVector 24

serve :: CoreOut -> TopQueue -> (CoreIn, TopQueue, Address, Maybe (Address, RGBValue))
serve (CoreOut _ _ _ p pt rdy) q = 
			( CoreIn { nextPack  = np' rdy
                      , dfuInstr = Nothing
                      , dfuData  = Nothing
                      , cfuInstr = Nothing
                      }
            , q' pt rdy
            , addr
            , px pt
            )
  where
    np' True        = Just (top q)
    np' _           = Nothing
    q' _ True       = pop q
    q' Core.Queue _ = push p q
    q' _ _          = q
    px Frame        = Just (addr, color) 
    px _            = Nothing 
    addr            = (16::Signed 32) * y + x
    x               = unpack $ bSlice d0 d32 $ pack $ unSF $ (p !! 0)
    y               = unpack $ bSlice d0 d32 $ pack $ unSF $ (p !! 1) :: Signed 32
    color           = bSlice d8 d24 (r+g+b)
    r               = bSlice d0 d32 $ pack $ unSF $ (p !! 2)
    g               = flip shiftL 8 $ bSlice d0 d32 $ pack $ unSF $ (p !! 3)
    b               = flip shiftL 16 $ bSlice d0 d32 $ pack $ unSF $ (p !! 4)
