

module Assembler (asm) where

import qualified Float
import CLaSH.Sized.Unsigned
import CLaSH.Class.BitPack
import CLaSH.Sized.Fixed
import CLaSH.Sized.Internal.Signed
import Data.String
import Data.List
import qualified Prelude (Float)
import Prelude hiding (Float) 

zeroes n = take n $ repeat '0'

asm f = case dropWhile (/='.') f of
	".dasm" -> asm' f assembleDI
	".masm" -> asm' f assembleDD
	_       -> error "Unrecognized file extension"

asm' :: String -> (String -> String) -> IO ()
asm' f ass = do
	f' <- readFile f
	writeFile (takeWhile (/='.') f ++ ".bin") (ass f')

assemble f s = fWordsInSrc (f gotos) s'
	where
		gotos = fmap (\(ln,l) -> (ln, drop 1 . concat . take 1 . takeWhile (isInfixOf "#") . words $ l)) (zipWith [0..] $ lines s)
		s' = fWordsInSrc (rmCmm . takeWhile (not . isInfixOf "#")) s
		rmCmm w:sw = if not $ isInfixOf ";" w then w:(rmCmm sw) else ""

assembleDI :: String -> String
assembleDI = assemble ltb
	where
		ltb "nz":w2:"SetVal":w4:w5:[] gt = "0000" ++ cp ++ "11" ++ rp ++ sp
		ltb "z" :w2:"SetVal":w4:w5:[] gt = "0001" ++ cp ++ "11" ++ rp ++ sp
			rp = (numToBin w3 :: KnownNat 3) ++ "0"
			sp = (numToBin w4 :: KnownNat 4)
			cp = (numToBin w4 :: KnownNat 4)
		ltb "a":"SetVal":w3:w4:[] gt = "0010000011" ++ rp ++ sp 
			rp = (numToBin w3 :: KnownNat 3) ++ "0"
			sp = (numToBin w4 :: KnownNat 4)
		ltb "nz":w2:w3:[] gt = "0000" ++ cp ++ act w3
		ltb "z" :w2:w3:[] gt = "0001" ++ cp ++ act w3
			cp = (numToBin w2 :: KnownNat 4)
			act "PushQ" = "1010101"
			act "PushF" = "1010101"
			act "Drop" = "1010101"
		ltb "next":w2:[] gt = "01" ++ idp
		ltb "val" :w2:[] gt = "0010" ++ val
		ltb "arg" :w2:[] gt = "0011" ++ pp
		ltb "a"   :w2:[] gt = "00100000" ++ act
			idp = (numToBin w2 :: KnownNat 3) ++ zeroes 11
			val = (numToBin w2 :: KnownNat 32)
			pp = (numToBin w2 :: KnownNat 16)
			act "PushQ" = "1010101"
			act "PushF" = "1010101"
			act "Drop" = "1010101"
		ltb "max"  :[] _ = "0000000" ++ zeroes 29
		ltb "min"  :[] _ = "0000001" ++ zeroes 29
		ltb "add"  :[] _ = "0000010" ++ zeroes 29
		ltb "sub"  :[] _ = "0000011" ++ zeroes 29
		ltb "mul"  :[] _ = "0000100" ++ zeroes 29
		ltb "div"  :[] _ = "0000101" ++ zeroes 29
		ltb "sqrt" :[] _ = "0000110" ++ zeroes 29
		ltb "abs"  :[] _ = "0000111" ++ zeroes 29
		ltb "floor":[] _ = "0001000" ++ zeroes 29

numToBin :: KnownNat a => String -> String
numToBin w 
	| ("." `isInfixOf` w || "e" `isInfixOf` w) = rationalToBinary w 
	| "-" `isInfixOf` w = sNToBinary w :: Signed a
        | otherwise = uNToBinary w :: Unsigned a

sNToBinary = clashToBin . (read :: String -> Signed a) 
uNToBinary = clashToBin . (read :: String -> Unsigned a) 
rationalToBinary = clashToBin . (fLitR :: Double -> Float.Float) . read

clashToBin = filter (/= '_') . show . pack

assembleDD :: String -> String
assembleDD = assemble wtb
	where
		wtb w
			| ("." `isInfixOf` w || "e" `isInfixOf` w) = rationalToBinary w 
			| "-" `isInfixOf` w = s32ToBinary w 
			| otherwise = u32ToBinary w 
