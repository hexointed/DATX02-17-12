module Assembler where

import qualified Float
import CLaSH.Sized.Unsigned
import CLaSH.Class.BitPack
import CLaSH.Sized.Fixed
import Data.String
import Data.List
import qualified Prelude (Float)
import Prelude hiding (Float) 

zeroes n = take n $ repeat '0'

assemble f = concat . fmap ((++"\n") . (concat . fmap f . words)) . lines

assembleDI :: String -> String
assembleDI = assemble wtb
	where
		wtb w 
			| w == "Continue" = "00"
			| w == "Next" = "01"
			| w == "Compute" = "10" ++ zeroes 34
			| w == "Done" = "11" ++ zeroes 34
			| w == "Left" = "0"
			| w == "Right" = "1"
			| w == "Max" = "000" ++ zeroes 30
			| w == "Min" = "001" ++ zeroes 30
			| w == "Add" = "010" ++ zeroes 30
			| w == "Sub" = "011" ++ zeroes 30
			| w == "Mul" = "100" ++ zeroes 30
			| w == "Div" = "101" ++ zeroes 30
			| w == "Sqrt" = "110" ++ zeroes 30
			| w == "Abs" = "111" ++ zeroes 30
			| w == "Val" = "0"
			| w == "Arg" = "1"
			| "pp" `isInfixOf` w = (u3ToBinary $ drop 2 w) ++ zeroes 29
			| "id" `isInfixOf` w = zeroes 18 ++ (u16ToBinary $ drop 2 w)
			| otherwise = rationalToBinary w

assembleDD :: String -> String
assembleDD = assemble wtb
	where
		wtb w = rationalToBinary w 

assembleCI :: String -> String
assembleCI = assemble wtb
	where
		wtb w 
			| w == "Instr" = ""
			| w == "Cond" = ""
			| w == "NZ" = "00"
			| w == "Z" = "01"
			| w == "A" = "10"
			| w == "PushF" = "00"
			| w == "PushQ" = "01"
			| w == "Drop" = "10"
			| w == "SetVal" = "11"
			| "cp" `isInfixOf` w = (u4ToBinary $ drop 2 w)
			| "pp" `isInfixOf` w = (u3ToBinary $ drop 2 w) ++ zeroes 1
			| "sp" `isInfixOf` w = (u4ToBinary $ drop 2 w)
			| "_" `isInfixOf` w = zeroes 10

clashToBin = filter (/= '_') . show . pack

u16ToBinary = clashToBin . (read :: String -> Unsigned 16) 
u4ToBinary = clashToBin . (read :: String -> Unsigned 4)
u3ToBinary = clashToBin . (read :: String -> Unsigned 3)
rationalToBinary = clashToBin . (fLitR :: Double -> Float.Float) . read

