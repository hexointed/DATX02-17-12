{-# LANGUAGE TemplateHaskell #-}
module Arith where

import Language.Haskell.TH
import Control.Monad
import CLaSH.Prelude
import Unsafe.Coerce
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

-- Bubble sort for 1 iteration
sortV xs = map fst sorted :< (snd (last sorted))
 where
   lefts  = head xs :> map snd (init sorted)
   rights = tail xs
   sorted = zipWith compareSwapL (lazyV lefts) rights

-- Compare and swap
compareSwapL a b = if a < b then (a,b)
                            else (b,a)

topEntity :: Vec 8 Bit -> Vec 8 Bit
topEntity = sortV

fold f a = undefined
	where
		len = lengthS a
