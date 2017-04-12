module Framebuffer where

import Base
import Core
import Pack
import DFU
import Float

type Pixel = (Unsigned 8, Unsigned 8, Unsigned 8)

framebuffer :: KnownNat n =>
	Vec (n + 1) (Signal CoreOut) -> 
	(Vec (n + 1) (Signal Bool), Signal (Vec 256 Pixel))

framebuffer reqs = (,)
	(unbundle $ fmap onehot (fmap fst selected))
	(mealy buffer (repeat (0,0,0)) (fmap snd selected))
	where
		selected = 
			fmap gather $
			choice hasFrame $
			fmap (\(i,o) -> fmap ((,) i) o) $
			imap (,) reqs
		onehot i = replace i True (repeat False)

hasFrame (i,l) (j,r) = case packType l of
	Frame -> (i, l)
	_     -> (j, r)

gather :: (i, CoreOut) -> (i, (Unsigned 32, Maybe Pixel))
gather (i,o) = (,) i $ case packType o of
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
