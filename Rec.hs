{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE IncoherentInstances #-}

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

realNewName n = do
	n' <- newName n
	return $ mkName (showName n')

insN n f fnName fnType = do
	className <- realNewName ("Class_" ++ fnName)
	tvName <- newName "a"
	tmpName <- newName fnName
	clas <- classD (return []) className [PlainTV tvName] []
		[sigD tmpName 
			(fnType (varT tvName))
		]
	let i n = do
		dec <- [d| 
				$(varP tmpName) = $(f n (varE tmpName)) 
			|]
		instType <- [t| 
				$(conT className) (Vec $(return . LitT . NumTyLit $ n) Bit) 
			|]
		return $ InstanceD 
			Nothing
			[] 
			instType
			dec
	xs <- sequence (P.map i [1 .. n])
	return $ (clas:xs)

tstN n f = do
	matches <- sequence (P.map matchN [1 .. n])
	return $ CaseE (VarE $ mkName "n") matches
		where
			matchN n = match (u n) (normalB f) []
			u 1 = [p| USucc UZero |]
			u n = [p| USucc $(u (n - 1)) |]
