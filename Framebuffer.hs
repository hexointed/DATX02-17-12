module Framebuffer where

import Base
import Core
import Pack
import DFU
import Float

type Pixel = (Unsigned 8, Unsigned 8, Unsigned 8)

framebuffer :: Vec (n + 1) (Signal CoreOut) -> Signal (Vec 256 Pixel)
framebuffer = 
	mealy buffer (repeat (0,0,0)) . 
	fmap gather . 
	choice hasFrame

hasFrame l r = case packType l of
	Frame -> l
	_     -> r

gather :: CoreOut -> (Unsigned 32, Maybe Pixel)
gather o = case packType o of
	Frame -> (bitCoerce $ shiftR (out !! 1) 16, Just . pixel $ out)
	_     -> (bitCoerce $ shiftR (out !! 1) 16, Nothing)
	where
		out = packOut o

pixel :: Pack -> Pixel
pixel a = (
		resize $ shiftR (bitCoerce $ a !! 2) 24,  
		resize $ shiftR (bitCoerce $ a !! 2) 16, 
		resize $ shiftR (bitCoerce $ a !! 2)  8
	)

buffer v (a, Nothing) = dup v
buffer v (a, Just px) = dup out
	where out = replace a px v
