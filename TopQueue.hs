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

serve :: TopQueue -> (Bool, Maybe Pack) -> (TopQueue, Maybe Pack)
serve q (rdy, pack) = mpop $ psh q
	where
		mpop = case rdy && not (isEmpty q) of
			True  -> (\q -> (pop q, Just $ top q))
			False -> (\q -> (q, Nothing))
		psh = case pack of
			Just p  -> push p
			Nothing -> id
