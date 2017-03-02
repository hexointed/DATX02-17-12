{-# LANGUAGE TemplateHaskell #-}

module Rec where

import Tmp
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Prelude ((++), ($), (.), div, (-), show)
import qualified Prelude as P
import CLaSH.Prelude hiding ((++))
import qualified CLaSH.Prelude ((++))

fullAdder a b c = (carry, sum)
	where
		i = xor a b
		sum = xor i c
		carry = (a .&. b) .|. (i .&. c)

_claAddN 1 rec =
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
_claAddN n rec = let (l,r) = (div n 2, n - l) in
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
				$(rec)
				$(rec)
				( SNat :: SNat $(return . LitT . NumTyLit $ l) )
	|]

insN n f fnName fnType = do
	className <- newName "A"
	tvName <- newName "a"
	tmpName <- newName fnName
	clas <- classD (return []) className [PlainTV tvName] []
		[sigD tmpName 
			(fnType (varT tvName))
		]
	base <- do
		n <- newName "n"
		dec <- [d|
				$(varP $ mkName fnName) = $(varE . mkName $ "undefined")
			|]
		predType <- [t| KnownNat $(varT n) |]
		instType <- [t|
				$(conT className) (Vec $(varT n) Bit)
			|]
		return $ InstanceD
			(Just Overlappable)
			[predType]
			instType
			dec
	let i n = do
		dec <- [d| 
				$(varP $ mkName fnName) = $(f n (varE . mkName $ fnName)) 
			|]
		instType <- [t| 
				$(conT className) (Vec $(return . LitT . NumTyLit $ n) Bit) 
			|]
		return $ InstanceD 
			(Just Overlapping)
			[] 
			instType
			dec
	xs <- sequence (P.map i [1 .. n])
	fn <- [d| $(varP $ mkName fnName) = $(varE tmpName) |]
	return (base:clas:xs)
