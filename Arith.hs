{-# LANGUAGE TemplateHaskell #-}

module Arith where

import CLaSH.Prelude
import Base
import Rec

rcAdder a b c = mapAccumR (\c (a, b) -> fullAdder a b c) c (zip a b)

claAdder :: forall n . KnownNat n => 
	Vec n Bit -> Vec n Bit -> Bit -> (Vec n Bit, Bit, Bit)
claAdder = 
	let 
		n = toUNat (SNat @n) 
	in
		$(makeRecursive 8 _claAddN "claAdder")

topEntity :: Vec 8 Bit -> Vec 8 Bit -> Bit -> (Vec 8 Bit, Bit, Bit)
topEntity = claAdder
