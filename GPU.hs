module GPU where

import Base
import Core
import DFU
import Pack
import Queue
import TopQueue
import Memory
import Framebuffer
import Float

import qualified Prelude as P

type Cores = 2

coreOut :: Vec Cores (Signal CoreOut)
coreOut = fmap (mealy step' initial') coreIn

coreIn :: Vec Cores (Signal CoreIn)
coreIn = fmap meld 
	(fmap (register (Nothing, False)) mealyQueue) 
	<*> (fmap (register (Nothing, Nothing)) mealyMemory)
	<*> mealyFrame

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

meld :: 
	Signal (Maybe Pack, Bool) -> 
	Signal (Maybe Instr, Maybe Float) -> 
	Signal Bool-> 
	Signal CoreIn
meld q m f = fmap mergeTuple q <*> m <*> f
	where mergeTuple (p,b) (i,f) fb = CoreIn p (b || fb) i f

zip' f a b = fmap f a <*> b

mealyFrame = fst fb

fb = framebuffer coreOut

showFrame = fmap show' (snd fb)
	where
		show' vs = 
			('\n':)$
			foldr (\a b -> a P.++ '\n':b) "" $
			fmap (foldr (:) "") $ 
			unconcat d16 $
			fmap showPixel vs 
		
		showPixel (0,0,0) = ' '
		showPixel _       = '\x2588'

simGPU = 
	sequence $ 
	fmap putStr $ 
	P.concatMap (P.take 1) . P.iterate (P.drop 500) $ 
	sample showFrame
