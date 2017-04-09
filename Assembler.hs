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

assemble f = concat . fmap ((++"\n") . (concat . fmap f . words)) . lines

asm f = case dropWhile (/='.') f of
	".dasm" -> asm' f assembleDI
	".masm" -> asm' f assembleDD
	_       -> error "Unrecognized file extension"

asm' :: String -> (String -> String) -> IO ()
asm' f ass = do
	f' <- readFile f
	writeFile (takeWhile (/='.') f ++ ".bin") (ass f')

assembleDI :: String -> String
assembleDI = assemble wtb
	where
		wtb w 
			| w == "Next"  = "10"
			| w == "Max"   = "0100000" ++ zeroes 29
			| w == "Min"   = "0100001" ++ zeroes 29
			| w == "Add"   = "0100010" ++ zeroes 29
			| w == "Sub"   = "0100011" ++ zeroes 29
			| w == "Mul"   = "0100100" ++ zeroes 29
			| w == "Div"   = "0100101" ++ zeroes 29
			| w == "Sqrt"  = "0100110" ++ zeroes 29
			| w == "Abs"   = "0100111" ++ zeroes 29
			| w == "Floor" = "0101000" ++ zeroes 29
			| w == "Val"   = "0110"
			| w == "Arg"   = "0111"
			| w == "NZ"    = "0000"
			| w == "Z"     = "0001"
			| w == "A"     = "00100000"
			| w == "PushF" = "00" ++ zeroes 26
			| w == "PushQ" = "01" ++ zeroes 26
			| w == "Drop"  = "10" ++ zeroes 26
			| w == "SetVal" = "11"
			| "pp" `isInfixOf` w = (u3ToBinary $ drop 2 w) ++ zeroes 29
			| "rp" `isInfixOf` w = (u3ToBinary $ drop 2 w) ++ "0"
			| "cp" `isInfixOf` w = (u4ToBinary $ drop 2 w)
			| "sp" `isInfixOf` w = (u4ToBinary $ drop 2 w) ++ zeroes 18
			| "id" `isInfixOf` w = (u16ToBinary $ drop 2 w) ++ zeroes 18
			| ("." `isInfixOf` w || "e" `isInfixOf` w) = rationalToBinary w 
			| "-" `isInfixOf` w = s32ToBinary w 
			| otherwise = u32ToBinary w 

assembleDD :: String -> String
assembleDD = assemble wtb
	where
		wtb w
			| ("." `isInfixOf` w || "e" `isInfixOf` w) = rationalToBinary w 
			| "-" `isInfixOf` w = s32ToBinary w 
			| otherwise = u32ToBinary w 

clashToBin = filter (/= '_') . show . pack

s32ToBinary = clashToBin . (read :: String -> Signed 32) 
u32ToBinary = clashToBin . (read :: String -> Unsigned 32) 
u16ToBinary = clashToBin . (read :: String -> Unsigned 16) 
u4ToBinary = clashToBin . (read :: String -> Unsigned 4)
u3ToBinary = clashToBin . (read :: String -> Unsigned 3)
rationalToBinary = clashToBin . (fLitR :: Double -> Float.Float) . read

