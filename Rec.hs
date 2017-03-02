{-# LANGUAGE TemplateHaskell #-}

module Rec (makeRecursive) where

import Tmp
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Prelude ((++), ($), (.), div, (-), show)
import qualified Prelude as P
import CLaSH.Prelude hiding ((++))

makeRecursive :: Integer -> (Integer -> Q Exp -> Q Exp) -> String -> Q Exp
makeRecursive nSteps f name = do
	matches <- sequence (P.map matchN [1 .. nSteps])
	return $ CaseE (VarE $ mkName "n") matches
		where
			matchN n = match (u n) (normalB (f n (varE $ mkName name))) []
			u 1 = [p| USucc UZero |]
			u n = [p| USucc $(u (n - 1)) |]
