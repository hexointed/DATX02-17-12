{-# LANGUAGE TemplateHaskell #-}

module Arith where

import Language.Haskell.TH
import Control.Monad
import CLaSH.Prelude
import Unsafe.Coerce
import Base
import Rec

rcAdder a b c = mapAccumR (\c (a, b) -> fullAdder a b c) c (zip a b)

claAdder :: forall n . KnownNat n => 
	Vec n Bit -> Vec n Bit -> Bit -> (Vec n Bit, Bit, Bit)
claAdder = 
	let 
		n = toUNat (SNat @n) 
	in
		$(tstN 32 [| test |])

topEntity :: Vec 3 Bit -> Vec 3 Bit -> Bit -> (Vec 3 Bit, Bit, Bit)
topEntity = claAdder

