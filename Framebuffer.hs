{-# LANGUAGE NoMonomorphismRestriction #-}

module Framebuffer where

import Base
import Core
import Pack
import DFU
import Float

type Pixel = (Unsigned 8, Unsigned 8, Unsigned 8)

framebuffer :: KnownNat n =>
	Signal (Unsigned 32) ->
	Vec (n + 1) (Signal CoreOut) -> 
	(Vec (n + 1) (Signal Bool), Signal Pixel)

framebuffer read reqs = (,)
	(unbundle $ fmap onehot selected)
	(buffer read $ fmap (fix . snd) selected)
	where
		selected = 
			fmap gather $
			choice hasFrame $
			fmap (\(i,o) -> fmap ((,) i) o) $
			imap (,) reqs
		onehot i = replace (fst i) (snd (snd i) /= Nothing) (repeat False)
		fix (i,p) = do
			p <- p
			return (i,p)

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

buffer :: (Num a, Enum a) => Signal a -> Signal (Maybe (a, Pixel)) -> Signal Pixel
buffer read px = setFirst (0,0,0) $ bram
	where 
		bram = blockRam init read px
		init = repeat (0,0,0) :: Vec 4096 Pixel
