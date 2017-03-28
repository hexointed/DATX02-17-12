{-# LANGUAGE FlexibleInstances #-}

module Float where

import CLaSH.Prelude

type GenFloat (a :: Nat) = Fixed Signed a a
type Float = GenFloat 32
type Point = Unsigned 4

instance KnownNat a => Floating (GenFloat a) where
	pi = undefined
	exp = undefined
	log = undefined
	sin = undefined
	cos = undefined
	asin = undefined
	acos = undefined
	atan = undefined
	sinh = undefined
	cosh = undefined
	asinh = undefined
	acosh = undefined
	atanh = undefined
