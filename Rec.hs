{-# LANGUAGE TemplateHaskell #-}

module Rec (makeRecursive) where

import qualified Prelude as P
import Language.Haskell.TH
import Control.Monad
import CLaSH.Prelude

makeRecursive :: Integer -> (Integer -> Q Exp -> Q Exp) -> String -> Q Exp
makeRecursive nSteps f name = do
	matches <- sequence (P.map matchN [1 .. nSteps])
	return $ CaseE (VarE $ mkName "n") matches
		where
			matchN n = match (u n) (normalB (f n (varE $ mkName name))) []
			u 1 = [p| USucc UZero |]
			u n = [p| USucc $(u (n - 1)) |]
