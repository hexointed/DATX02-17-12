{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Oled where

import Base
import GPU
import Framebuffer
import CLaSH.Sized.Internal.BitVector

showFrame =
	fmap (\px -> if px == (0,0,0) then high else low ) (snd fb')

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


