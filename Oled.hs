{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Oled where

import Base
import GPU
import Framebuffer
import CLaSH.Sized.Internal.BitVector

showFrame =
	fmap (\px -> if px == (0,0,0) then high else low ) $
	snd $ fb (fmap (resize . pxAddr . scan) inc :: Signal (Unsigned 32))

inc :: Signal (Unsigned 12)
inc = mealy (\i _ -> (i+1, i) ) 0 (pure 0)

scan n = (x, y + y')
	where
		x  = (n `div` 8) `mod` 128
		y  = n `mod` 8
		y' = 8 * div n 1024

pxAddr (x, y) = 4095 - (x + y * 128)

accum8 (n, v) b = (,) (n', v') (send, v')
	where
		send
			| n == 7    = high
			| otherwise = low
		v' = replaceBit n b v :: BitVector 8
		n' = n + 1 :: Unsigned 3

topEntity = 
	fmap (\(a, b) -> (\i -> (i,a,b))) (mealy accum8 (7, 0) showFrame) <*>
	fmap (\a -> 
			let (x,y) = scan a
			in resize (shiftL (shiftR y 3) 7 .|. x :: Unsigned 12) :: Unsigned 9
		) inc


