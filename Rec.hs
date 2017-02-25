{-# LANGUAGE TemplateHaskell #-}

module Rec where

import Language.Haskell.TH
import Control.Monad
import Prelude

curryN :: Int -> Q Exp
curryN n = do
	f <- newName "f"
	xs <- replicateM n (newName "x")
	let
		args = map VarP (f:xs)
		nutp = TupE (map VarE xs)
	return $ LamE args (AppE (VarE f) nutp)
