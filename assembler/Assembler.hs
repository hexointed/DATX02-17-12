module Assembler where

import CLaSH.Prelude (Unsigned, Fixed, Signed, fLitR, pack)
import Data.List
import Data.Char
import Control.Arrow (left)
import Prelude hiding (Word, Float, break)

import Inst

data TextType
	= Data [Word] 
	| Text [Word]

type Float = Fixed Signed 16 16

asm f = do
	file <- fmap lines $ readFile f
	let
		(dat,txt, labels) = calcLables $ splitSections file
		dataOut = makeData dat
		newC = 
			fmap (fmap assembleInst) $ 
			cleanupCode $ 
			replaceLabels labels file
	case compile newC of
		Right code -> do
			writeFile 
				"dfuIMemory.bin" 
				(filter (/='_') $ unlines $ fmap show code)
			writeFile "dfuDMemory.bin" dataOut
		Left error -> putStrLn error
	return ()

compile = 
	sequence . 
	map (\(l,c) -> left (("Line " ++ show l ++ ": ") ++) c)

calcLables :: ([Line], [Line]) -> ([Line], [Line], [(String, Int)])
calcLables (dat, txt) = (dat', txt, ldat)
	where
		ldat = getLabels 0 dat'
		ltxt = getLabels 0 txt
		dat' = dat ++ concat (map (\(str,val) -> [str ++ ":", show val]) ltxt)

getLabels :: Int -> [Line] -> [(String,Int)]
getLabels count [] = []
getLabels count (l:ls)
	| words l == [] = getLabels count ls
	| isLabel l = (head (words l) \\ ":", count) : getLabels count ls
	| otherwise = getLabels (count + 1) ls

isLabel w = ":" `isInfixOf` w

replaceLabels :: [(String,Int)] -> [Line] -> [Line]
replaceLabels m w = map (unwords . map replace . words) w
	where
		replace w
			| "&" `isPrefixOf` w = case lookup (tail w) m of
				Just i  -> show i
				Nothing -> w
			| otherwise         = w

splitSections :: [Line] -> ([Line], [Line])
splitSections [] = ([],[])
splitSections (l:ls)
	| head l == '.' = case l of
		".data:" -> (p ++ d', t')
		".text:" -> (d', p ++ t')
		_       -> error l
	| otherwise = error "Expected directive"
	where
		p = takeWhile (not . isPrefixOf ".") ls
		d = dropWhile (not . isPrefixOf ".") ls
		(d', t') = splitSections d

makeData :: [Line] -> String
makeData tt = 
	unlines $ 
	map datToBin $ 
	filter (not . isLabel) $
	filter (\l -> words l /= []) tt 
	where
		undata (Data d) = d
		isData (Data d) = True
		isData _        = False

datToBin :: String -> String
datToBin w 
	| ("." `isInfixOf` w || "e" `isInfixOf` w) = rationalToBinary w 
 	| otherwise = u32ToBinary w

u32ToBinary = clashToBin . (read :: String -> Unsigned 32) 
rationalToBinary = clashToBin . (fLitR :: Double -> Float) . read

clashToBin = filter (/= '_') . show . pack
