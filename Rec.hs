{-# LANGUAGE TemplateHaskell #-}

module Rec where

import Language.Haskell.TH
import Control.Monad
import Prelude ((++), ($), (.), div, (-), show)
import CLaSH.Prelude hiding ((++))
import qualified CLaSH.Prelude ((++))

fullAdder a b c = (carry, sum)
	where
		i = xor a b
		sum = xor i c
		carry = (a .&. b) .|. (i .&. c)

_claAdd0 :: Vec 0 Bit -> Vec 0 Bit -> Bit -> (Vec 0 Bit, Bit, Bit)
_claAdd0 a b carry = (sum, propagate, generate)
	where
		sum = Nil
		propagate = high
		generate = low

_claAddN 1 =
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
_claAddN n =
	let
		(l,r) = (div n 2, n - l)
		(l',r') = (show l, show r)
		n' = show n
	in
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
					$(varE . mkName $ "_claAdd" ++ l')
					$(varE . mkName $ "_claAdd" ++ r')
					$(varE . mkName $ "d" ++ l')
		|]
