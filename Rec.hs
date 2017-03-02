{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Rec where

import qualified Prelude as P
import Language.Haskell.TH
import Control.Monad
import CLaSH.Prelude

class ToQ a where
	exp' :: a -> Exp
	typ' :: a -> Type
	pat' :: a -> Pat
	bod' :: a -> Body
	bod' = NormalB . exp'

class GenQ a where
	genQ :: ToQ b => b -> a

instance GenQ Exp where
	genQ = exp'

instance GenQ Type where
	genQ = typ'

instance GenQ Pat where
	genQ = pat'

instance GenQ Body where
	genQ = bod'

instance GenQ a => GenQ (Q a) where
	genQ = return . genQ

instance ToQ String where
	exp' = VarE . mkName
	typ' = VarT . mkName
	pat' = VarP . mkName

instance ToQ Integer where
	exp' = LitE . IntegerL
	typ' = LitT . NumTyLit
	pat' = LitP . IntegerL

snatQ :: Integer -> Q Exp
snatQ d = [| SNat :: SNat $(genQ d) |]

makeRecursive :: Integer -> (Integer -> Q Exp -> Q Exp) -> String -> Q Exp
makeRecursive nSteps f name = do
	matches <- sequence (P.map matchN [0 .. nSteps])
	return $ CaseE (genQ "n") matches
		where
			matchN n = match (u n) (normalB (f n (genQ name))) []
			u 0 = [p| UZero |]
			u n = [p| USucc $(u (n - 1)) |]
