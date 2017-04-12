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

coreOut :: Vec 1 (Signal CoreOut)
coreOut = fmap (mealy step' initial') coreIn

coreIn :: Vec 1 (Signal CoreIn)
coreIn = zip' meld 
	(fmap (register Nothing) mealyQueue) 
	(fmap (register (Nothing, Nothing)) mealyMemory)

mealyQueue :: Vec 1 (Signal (Maybe Pack))
mealyQueue = repeat (mealy serve (push (0 :> 256 :> repeat 0) empty)) <*>
	(fmap (fmap (\c -> (wantPack c, pack' c))) coreOut)
	where
		pack' c = case packType c of
			DFU.Queue -> Just $ packOut c
			_         -> Nothing

mealyMemory :: Vec 1 (Signal (Maybe Instr, Maybe Float))
mealyMemory = 
	repeat (mealy fetch () . fmap (\c -> (dfuIPtr c, dfuDPtr c))) <*>
	coreOut

meld :: Signal (Maybe Pack) -> Signal (Maybe Instr, Maybe Float) -> Signal CoreIn
meld q m = zip' (uncurry . CoreIn) q m <*> pure True

zip' f a b = fmap f a <*> b

mealyFrame = fmap show' fb
	where
		fb = framebuffer coreOut

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
	sample mealyFrame
