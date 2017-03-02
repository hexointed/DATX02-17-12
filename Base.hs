{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Base where

import CLaSH.Prelude
import Language.Haskell.TH
import Control.Monad
import Rec

or' = (.|.)
and' = (.&.)
xor' = xor
not' = complement

fullAdder a b c = (carry, sum)
	where
		i = xor a b
		sum = xor i c
		carry = (a .&. b) .|. (i .&. c)

_claAddN 1 rec =
	[|
		let
			claAdd1_ :: Vec 1 Bit -> Vec 1 Bit -> Bit -> (Vec 1 Bit, Bit, Bit)
			claAdd1_ (a:>Nil) (b:>Nil) carry =
				let
					(_, sum) = fullAdder a b carry
					propagate = a .|. b
					generate = a .&. b
				in
					(sum :> Nil, propagate, generate)
		in
			claAdd1_
	|]
_claAddN n rec = let (l,r) = (div n 2, n - l) in
	[|
		let
			claAddN_ f1 f2 s a b carry =
				let
					(al, ar) = splitAt s a
					(bl, br) = splitAt s b
					(s0, p0, g0) = f2 ar br carry
					(s1, p1, g1) = f1 al bl (carry .&. p0 .|. g0)
					sum = s1 CLaSH.Prelude.++ s0
					propagate = p0 .&. p1
					generate = g0 .&. p1 .|. g1
				in
					(sum, propagate, generate)
		in
			claAddN_
				$(rec)
				$(rec)
				( SNat :: SNat $(return . LitT . NumTyLit $ l) )
	|]
