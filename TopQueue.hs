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

type VecIn n = Vec n (Bool, Maybe Pack)
type VecOut n = Vec n (Maybe Pack, Bool)
type IndexSize = Unsigned 8

serve :: KnownNat n => TopQueue -> VecIn (n+1) -> (TopQueue, VecOut (n+1))
serve q ps = (q'', injectL rdyindex mpout $ injectR mpindex acc $ repeat (Nothing, False)) 
	where
		(mpindex, mpin) = firstmp ps
		(rdyindex, rdy) = firstrdy ps
		(q', acc) = case mpin of
			Just p -> (push p q, True)
			otherwise -> (q, False)
		(q'', mpout) = case rdy of
			True -> (pop q', topOfq q')
			otherwise -> (q', Nothing)

topOfq :: TopQueue -> Maybe Pack
topOfq q = if isEmpty q then Nothing else Just (top q)

firstmp :: KnownNat n => VecIn (n+1) -> (IndexSize, Maybe Pack)
firstmp ps = fold sel (imap(\i (_,mp) -> (fromIntegral i,mp)) ps)
	where
                sel (i, Just p) _           = (i, Just p) 
                sel _           (i, Just p) = (i, Just p)
                sel _           _           = (0, Nothing)

firstrdy :: KnownNat n => VecIn (n+1) -> (IndexSize, Bool)
firstrdy ps = fold sel (imap(\i (rdy,_) -> (fromIntegral i,rdy)) ps)
	where
                sel (i, True) _         = (i, True) 
                sel _         (i, True) = (i, True)
                sel _         _         = (0, False)

injectL :: KnownNat n => IndexSize -> Maybe Pack -> VecOut (n+1) -> VecOut (n+1)
injectL i mp ps = replace i (mp, r) ps
	where (_,r) = ps !! i

injectR :: KnownNat n => IndexSize -> Bool -> VecOut (n+1) -> VecOut (n+1)
injectR i rdy ps = replace i (l, rdy) ps
	where (l,_) = ps !! i
