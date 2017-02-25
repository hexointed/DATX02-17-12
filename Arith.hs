{-# LANGUAGE TemplateHaskell #-}
module Arith where

import Language.Haskell.TH
import Control.Monad
import CLaSH.Prelude
import Base
import Rec

fullAdder a b c = (carry, sum)
	where
		i = xor' a b
		sum = xor' i c
		carry = or' (and' a b) (and' i c)

rcAdder a b c = mapAccumR (\c (a, b) -> fullAdder a b c) c (zip a b)

claAdder a b c = undefined
	where
		single = undefined

claPropGen p g = foldr 
	(\(p1,g1) (p2,g2) -> (and' p1 p2, or' (and' g1 p2) g2)) (high, low) 
	(zip p g)

topEntity :: Vec 2 Bit -> Bit
topEntity = undefined

fold f a = undefined
	where
		len = lengthS a

$(do 
	fn <- curryN 2
	return [FunD (mkName "curry") [Clause [] (NormalB fn) []]]
	)
