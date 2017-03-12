{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module Float where

import Base

type GenFloat (a :: Nat) = Fixed Signed a a
type Float = GenFloat 32

--inf :: KnownNat a => GenFloat a
--inf = Fixed (-1)

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
