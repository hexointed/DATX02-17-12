module GPU where

import Base
import Core
import DFU
import Pack
import Queue
import TopQueue
import Memory
import Float

import qualified Prelude as P

coreOut = mealy step' initial' coreIn

coreIn :: Signal CoreIn
coreIn = meld 
	(register Nothing mealyQueue) 
	(register (Nothing, Nothing) mealyMemory)

mealyQueue = mealy serve 
	(push (0 :> 256 :> repeat 0) empty) 
	(fmap (\c -> (wantPack c, pack' c)) coreOut)
	where
		pack' c = case packType c of
			DFU.Queue -> Just $ packOut c
			_         -> Nothing

mealyMemory = mealy fetch () $ 
	fmap (\c -> (dfuIPtr c, dfuDPtr c)) coreOut

meld q m = fmap (uncurry . CoreIn) q <*> m

mealyFrame = 
	fmap pprint $ 
	(mealy f' (repeat False) coreOut :: Signal (Vec 256 Bool))
	where
		f' frame cin = case packType cin of
			DFU.Frame -> (replace (shiftR (p' !! 1) 16) (p' !! 4 == 0) frame, frame)
			_         -> (frame, frame)
			where p' = packOut cin
		pprint p = 
			(P.++"\n") $
			foldr (\a b -> a P.++ "\n" P.++ b) "" $ 
			map (foldr (\a b -> show' a P.++ b) "") $ 
			unconcat d16 p
		show' True = " "
		show' False = "\x2588"

simGPU = 
	sequence $ 
	fmap putStr $ 
	P.concatMap (P.take 1) . P.iterate (P.drop 500) $ 
	sample mealyFrame
