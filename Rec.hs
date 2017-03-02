{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE IncoherentInstances #-}

module Rec (makeRec, caseWith) where

import Tmp
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Prelude ((++), ($), (.), div, (-), show)
import qualified Prelude as P
import CLaSH.Prelude hiding ((++))
import qualified CLaSH.Prelude ((++))

realNewName n = do
	n' <- newName n
	return $ mkName (showName n')

makeRec n f fnName fnType = do
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

caseWith n f = do
	matches <- sequence (P.map matchN [1 .. n])
	return $ CaseE (VarE $ mkName "n") matches
		where
			matchN n = match (u n) (normalB f) []
			u 1 = [p| USucc UZero |]
			u n = [p| USucc $(u (n - 1)) |]
