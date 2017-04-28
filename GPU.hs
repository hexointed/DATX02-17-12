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

type Cores = 4

coreOut :: Vec Cores (Signal CoreOut)
coreOut = fmap (mealy step' initial') coreIn

coreIn :: Vec Cores (Signal CoreIn)
coreIn = pure meld 
	<*> fmap (register (Nothing, False)) mealyQueue
	<*> fmap (register (Nothing, Nothing)) mealyMemory
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

mealyFrame = fst fb'

fb' = fb (fmap (resize . pxAddr . scan) inc :: Signal (Unsigned 32))

fb read = framebuffer read $ fmap (register (CoreOut 0 0 (repeat 0) None False)) coreOut

inc :: Signal (Unsigned 12)
inc = mealy (\i _ -> (i+1, i) ) 0 (pure 0)

scan n = (x, y + y')
	where
		x  = (n `div` 8) `mod` 128
		y  = n `mod` 8
		y' = 8 * div n 1024

pxAddr (x, y) = 4095 - (x + y * 128)
