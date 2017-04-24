module Simulate where

import Base hiding (Bits)
import Framebuffer
import GPU
import qualified Prelude as P

type Bits = 8
type Pixels = 256

showFrame = 
	fmap show' $
	mealy simDisplay (repeat (0,0,0), 0) $
	discardFirst $
	snd $ fb (fmap resize inc :: Signal (Unsigned 32))
	where
		show' vs = 
			('\n':)$
			foldr (\a b -> a P.++ '\n':b) "" $
			fmap (foldr (:) "") $ 
			unconcat d64 $
			fmap showPixel vs 
		
		showPixel (0,0,0) = ' '
		showPixel _       = '\x2588'

simDisplay :: (Vec Pixels Pixel, Unsigned Bits) -> (Bool, Pixel, Unsigned Bits) ->
	((Vec Pixels Pixel, Unsigned Bits), Vec Pixels Pixel)

simDisplay (disp, i') (r, p, i) = case r of
	False -> ((disp, i), disp)
	True -> ((disp', i), disp')
		where
			disp' = replace i' p disp

discardFirst p = 
	register (False, (0,0,0), 0) $ 
	signal ((,,) True) <*> p <*> inc

inc :: Signal (Unsigned Bits)
inc = mealy (\i _ -> (i+1, i) ) 0 (pure 0)

simGPU = 
	sequence $ 
	fmap putStr $ 
	P.concatMap (P.take 1) . P.iterate (P.drop 500) $ 
	sample showFrame
