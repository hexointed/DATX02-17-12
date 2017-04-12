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

type Cores = 1

coreOut :: Vec Cores (Signal CoreOut)
coreOut = fmap (mealy step' initial') coreIn

coreIn :: Vec Cores (Signal CoreIn)
coreIn = zip' meld 
	(fmap (register (Nothing, False)) mealyQueue) 
	(fmap (register (Nothing, Nothing)) mealyMemory)

mealyQueue :: Vec Cores (Signal (Maybe Pack, Bool))
mealyQueue = mealyB serve (push (0 :> 256 :> repeat 0) empty) wiw
	where
		wiw = fmap (fmap (\co -> (wantPack co, pack' co))) coreOut
		pack' co = case packType co of
			DFU.Queue -> Just $ packOut co
			_         -> Nothing

mealyMemory :: Vec Cores (Signal (Maybe Instr, Maybe Float))
mealyMemory = 
	repeat (mealy fetch () . fmap (\c -> (dfuIPtr c, dfuDPtr c))) <*>
	coreOut

meld :: Signal (Maybe Pack, Bool) -> Signal (Maybe Instr, Maybe Float) -> Signal CoreIn
meld q m = zip' mergeTuple q m
	where mergeTuple (p,b) (i,f) = CoreIn p b i f

zip' f a b = fmap f a <*> b

mealyFrame = 
	fmap pprint $ 
	(mealy f' (repeat False) (head coreOut) :: Signal (Vec 256 Bool))
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
