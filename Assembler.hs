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
	".casm" -> asm' f assembleCI
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
			| w == "Next"     = "01"
			| w == "Compute"  = "10" ++ zeroes 34
			| w == "Done"     = "11" ++ zeroes 34
			| w == "Max"  = "000000" ++ zeroes 30
			| w == "Min"  = "000001" ++ zeroes 30
			| w == "Add"  = "000010" ++ zeroes 30
			| w == "Sub"  = "000011" ++ zeroes 30
			| w == "Mul"  = "000100" ++ zeroes 30
			| w == "Div"  = "000101" ++ zeroes 30
			| w == "Sqrt" = "000110" ++ zeroes 30
			| w == "Abs"  = "000111" ++ zeroes 30
			| w == "Val"  = "0010"
			| w == "Arg"  = "0011"
			| "pp" `isInfixOf` w = (u3ToBinary $ drop 2 w) ++ zeroes 29
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

assembleCI :: String -> String
assembleCI = assemble wtb
	where
		wtb w 
			| w == "NZ" = "00"
			| w == "Z"  = "01"
			| w == "A"  = "100000"
			| w == "PushF" = "00" ++ zeroes 8
			| w == "PushQ" = "01" ++ zeroes 8
			| w == "Drop"  = "10" ++ zeroes 8
			| w == "SetVal" = "11"
			| "cp" `isInfixOf` w = (u4ToBinary $ drop 2 w)
			| "pp" `isInfixOf` w = (u3ToBinary $ drop 2 w) ++ zeroes 1
			| "sp" `isInfixOf` w = (u4ToBinary $ drop 2 w)

clashToBin = filter (/= '_') . show . pack

s32ToBinary = clashToBin . (read :: String -> Signed 32) 
u32ToBinary = clashToBin . (read :: String -> Unsigned 32) 
u16ToBinary = clashToBin . (read :: String -> Unsigned 16) 
u4ToBinary = clashToBin . (read :: String -> Unsigned 4)
u3ToBinary = clashToBin . (read :: String -> Unsigned 3)
rationalToBinary = clashToBin . (fLitR :: Double -> Float.Float) . read

