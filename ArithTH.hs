{-# LANGUAGE TemplateHaskell #-}

module ArithTH where

import qualified Prelude as P
import CLaSH.Prelude hiding (snat)
import Language.Haskell.TH
import Control.Monad
import Rec

_claAddN 0 rec = [| (\a b c -> (Nil, high, low)) |]
_claAddN 1 rec = [| (\a' b' c -> 
		let a = head a' ; b = head b' in
			(singleton $ snd $ fullAdder a b c, a .|. b, a .&. b)
	) |]
_claAddN n rec = let (l,r) = (div n 2, n - l) in
	[|
		(\f1 f2 s a b carry ->
			let
				(al, ar) = splitAt s a
				(bl, br) = splitAt s b
				(s0, p0, g0) = f2 ar br carry
				(s1, p1, g1) = f1 al bl (carry .&. p0 .|. g0)
				sum = s1 ++ s0
				propagate = p0 .&. p1
				generate = g0 .&. p1 .|. g1
			in
				(sum, propagate, generate)
		) $(rec) $(rec) $(snatQ l)
	|]

mW = do
	d0 <- sequence $ prod 1
	[d| $(genQ "test") = $(lamE [genQ "a", genQ "b"] (return $ (P.!!1) $ d0)) |]

prod n = P.map (\(l,r) -> mul l r) $ P.map (\k -> (k,n-k)) [0 .. n] 
mul l r = [| at $(snatQ l) a .&. at $(snatQ r) b |]
