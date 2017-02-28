{-# LANGUAGE TemplateHaskell #-}

module Rec where

import Language.Haskell.TH
import Control.Monad
import Prelude
import qualified CLaSH.Prelude as C

fullAdder a b c = (carry, sum)
	where
		i = C.xor a b
		sum = C.xor i c
		carry = (a C..&. b) C..|. (i C..&. c)

_claAdd0 :: C.Vec 0 C.Bit -> C.Vec 0 C.Bit -> C.Bit -> (C.Vec 0 C.Bit, C.Bit, C.Bit)
_claAdd0 a b carry = (sum, propagate, generate)
	where
		sum = C.Nil
		propagate = C.high
		generate = C.low

_claAdd1 :: C.Vec 1 C.Bit -> C.Vec 1 C.Bit -> C.Bit -> (C.Vec 1 C.Bit, C.Bit, C.Bit)
_claAdd1 (a C.:> C.Nil) (b C.:> C.Nil) carry = (sum C.:> C.Nil, propagate, generate)
	where
		(_, sum) = fullAdder a b carry
		propagate = a C..|. b
		generate = a C..&. b

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
						(al, ar) = C.splitAt s a
						(bl, br) = C.splitAt s b
						(s0, p0, g0) = f2 ar br carry
						(s1, p1, g1) = f1 al bl (carry C..&. p0 C..|. g0)
						sum = s1 C.++ s0
						propagate = p0 C..&. p1
						generate = g0 C..&. p1 C..|. g1
					in
						(sum, propagate, generate)
			in
				claAddN_
					$(varE . mkName $ "_claAdd" ++ l')
					$(varE . mkName $ "_claAdd" ++ r')
					$(varE . mkName $ "d" ++ l')
		|]
