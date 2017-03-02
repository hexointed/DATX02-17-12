{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Arith where

import Language.Haskell.TH
import Control.Monad
import CLaSH.Prelude
import Unsafe.Coerce
import Base
import Rec

rcAdder a b c = mapAccumR (\c (a, b) -> fullAdder a b c) c (zip a b)

$(makeRec
		32
		_claAddN
		"_claAdder"
		(\t -> [t| $(t) -> $(t) -> Bit -> ( $(t), Bit, Bit ) |] )
	)

claAdder :: forall n . KnownNat n => 
	Vec n Bit -> Vec n Bit -> Bit -> (Vec n Bit, Bit, Bit)
claAdder = 
	let 
		n = toUNat (SNat @n) 
	in
		$(caseWith 32 [| _claAdder |])

topEntity :: Vec 32 Bit -> Vec 32 Bit -> Bit -> (Vec 32 Bit, Bit, Bit)
topEntity = claAdder
