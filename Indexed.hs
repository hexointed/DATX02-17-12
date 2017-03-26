{-# LANGUAGE FlexibleInstances #-}

module Indexed where

import CLaSH.Prelude

class Indexed a where
	type Size a :: Nat

type Ptr a = Unsigned (Size a)

instance KnownNat n => Indexed (Vec n a) where
	type Size (Vec n a) = CLog 2 n
