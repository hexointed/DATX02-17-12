module Assembler where

import CLaSH.Prelude hiding ((++), map, last)
import Data.List
import Data.Char
import qualified Float
import Prelude

data TextType = Data [[String]] | Code [[String]]

break = fmap words . lines . map toLower
glue = unlines . fmap unwords

pieceUp :: [[String]] -> [TextType]
pieceUp ((".data":ws):ls) = (Data (findThisPiece ls)):(pieceUp ls)
pieceUp ((".text":ws):ls) = (Code (findThisPiece ls)):(pieceUp ls)
pieceUp (l:ls) = pieceUp ls
pieceUp [] = []

findThisPiece :: [[String]] -> [[String]]
findThisPiece ((".data":ws):ls) = []
findThisPiece ((".text":ws):ls) = []
findThisPiece (([]):ls) = findThisPiece ls
findThisPiece (l:ls) = l:(findThisPiece ls)
findThisPiece [] = []

getGotos :: Int -> Int -> [TextType] -> [(String,Int)]
getGotos dln cln ((Data ls):ts) = gt ++ (getGotos nln cln ts)
	where (nln, gt) = makeGotos dln ls
getGotos dln cln ((Code ls):ts) = gt ++ (getGotos dln nln ts)
	where (nln, gt) = makeGotos cln ls
getGotos _ _ [] = []

makeGotos ln ls = (nln, gt)
	where 
		(_,nln) = last gt
		gt = makeGotos' ln ls
makeGotos' :: Int -> [[String]] -> [(String,Int)]
makeGotos' ln ((w:[]):l:ls)| isGoto w = (w \\ ":", ln):(makeGotos' (ln+1) ls)
makeGotos' ln (l:ls) = (makeGotos' (ln+1) ls)
makeGotos' _ [] = []
isGoto w = ":" `isInfixOf` w

onlyData :: [TextType] -> [[String]]
onlyData ((Data ls):ts) = ls ++ (onlyData ts)
onlyData (_:ts) = onlyData ts
onlyData [] = []
onlyCode :: [TextType] -> [[String]]
onlyCode ((Code ls):ts) = ls ++ (onlyCode ts)
onlyCode (_:ts) = onlyCode ts
onlyCode [] = []

replaceGotos :: [(String,Int)] -> [[String]] -> [[String]]
replaceGotos gt ((ws):ls) = (fmap (\w -> if isInfixOf "&" w then show $ lookup (w \\ "&") gt else w) ws):(replaceGotos gt ls)

makeData :: [[String]] -> String
makeData = glue . fmap (fmap (\w -> if not $ isGoto w then datToBin w else w))


datToBin :: String -> String
datToBin w 
	| ("." `isInfixOf` w || "e" `isInfixOf` w) = rationalToBinary w 
	| "-" `isInfixOf` w = s32ToBinary w
        | otherwise = u32ToBinary w

s32ToBinary = clashToBin . (read :: String -> Signed 32) 
u32ToBinary = clashToBin . (read :: String -> Unsigned 32) 
rationalToBinary = clashToBin . (fLitR :: Double -> Float.Float) . read

clashToBin = filter (/= '_') . show . pack
