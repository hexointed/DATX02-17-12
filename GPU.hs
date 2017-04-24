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

type Cores = 1

coreOut :: Vec Cores (Signal CoreOut)
coreOut = fmap (mealy step' initial') coreIn

coreIn :: Vec Cores (Signal CoreIn)
coreIn = fmap meld 
	mealyQueue
	<*> (fmap (register (Nothing, Nothing)) mealyMemory)
	<*> mealyFrame

mealyQueue :: Vec Cores (Signal (Maybe Pack, Bool))
mealyQueue = unbundle $ topQueue $ bundle wiw
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

mealyFrame = fst $ fb (pure 0)

fb read = framebuffer read $ fmap (register (CoreOut 0 0 (repeat 0) None False)) coreOut
