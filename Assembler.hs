module Assembler where

import CLaSH.Prelude (Unsigned, Fixed, Signed, fLitR, pack)
import Data.List
import Data.Char
import Control.Arrow (left)
import Prelude hiding (Word, Float, break)

import Inst

type Error a = Either String a
type Float = Fixed Signed 16 16

asm f = do
	file <- fmap (fmap (takeWhile (/=';'))) $ fmap lines $ readFile f
	let
		(dat,txt, labels) = calcLables $ splitSections file
		dataOut = makeData dat
		newC = 
			fmap (fmap assembleInst) $ 
			cleanupCode $ 
			replaceLabels labels txt
	case compile newC of
		Right code -> do
			writeFile 
				"dfuIMemory.dat" 
				(filter (/='_') $ unlines $ fmap show code)
			writeFile "dfuDMemory.dat" dataOut
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
splitSections (l:ls) = case l of
		".data:" -> (p ++ d', t')
		".text:" -> (d', p ++ t')
		_        -> error $ "Expected section specifier, found: " ++ l
		where
		p = takeWhile (not . isSection) ls
		d = dropWhile (not . isSection) ls
		(d', t') = splitSections d
		isSection a = ".text:" `isPrefixOf` a || ".data:" `isPrefixOf` a

makeData :: [Line] -> String
makeData tt = 
	unlines $ 
	map datToBin $ 
	filter (not . isLabel) $
	filter (\l -> words l /= []) tt 

datToBin :: String -> String
datToBin w 
	| ("." `isInfixOf` w || "e" `isInfixOf` w) = rationalToBinary w 
	| otherwise = u32ToBinary w

u32ToBinary str = clashToBin $ case reads str of
	[(n, "")] -> n :: Unsigned 32
	_         -> error $ "Parse Error: " ++ str

rationalToBinary str = clashToBin $ case reads str of
	[(n, "")] -> fLitR (n :: Double) :: Float
	_         -> error $ "Parse Error: " ++ str

clashToBin = filter (/= '_') . show . pack
