module GPU where

import Base
import Core
import CFU
import Pack
import Queue
import TopQueue
import Memory

import qualified Prelude as P

coreOut = mealy step' initial' coreIn

coreIn = meld 
	(register Nothing mealyQueue) 
	(register (Nothing, Nothing, Nothing) mealyMemory)

mealyQueue = mealy serve 
	(filled $ 0 :> repeat 0) 
	(fmap (\c -> (ready c, pack' c)) coreOut)
	where
		pack' c = case packType c of
			CFU.Queue -> Just $ packOut c
			_          -> Nothing

mealyMemory = mealy fetch () $ 
	fmap (\c -> (dfuIPtr c, dfuDPtr c, cfuIPtr c)) coreOut

meld q m = fmap (curry3 . CoreIn) q <*> m

mealyFrame = fmap pprint $ (mealy f' (repeat False) coreOut :: Signal (Vec 64 Bool))
	where
		f' frame cin = case packType cin of
			CFU.Frame -> (replace (packOut cin !! 1) (packOut cin !! 2 == 0) frame, frame)
			_         -> (frame, frame)
		pprint p = 
			(P.++"\n") $
			foldr (\a b -> a P.++ "\n" P.++ b) "" $ 
			map (foldr (\a b -> show' a P.++ b) "") $ 
			unconcat d8 p
		show' True = " "
		show' False = "\x2588"
