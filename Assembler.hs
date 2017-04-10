{-# LANGUAGE MultiWayIf #-}

module Assembler where

import CLaSH.Prelude hiding ((++), last, tail, Word, lines, zip, map)
import Data.List
import Data.Char
import qualified Float
import Prelude hiding (Word)

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
-- =======

type Bits' n = Either String (BitVector n)
type Line = String
type Word = String

cleanupCode :: String -> [(Int, Line)]
cleanupCode = 
	filter (not . isDirective . snd) .
	filter (not . isEmpty . snd) .
	map filterLabels .
	zip [1 ..] .
	lines
	where
		isEmpty line = words line == []

		isDirective ('.':line) = True
		isDirective _          = False

		filterLabels (nr, line)
			| ":" `isInfixOf` line = (,) nr $ tail $ dropWhile (/=':') line
			| otherwise            = (nr, line)

assembleInst :: Line -> Bits' 16
assembleInst str = case words str of
	[]   -> Left "Expected instruction, found nothing"
	w:ws -> if
		| isCond w    -> assembleCFU (w:ws)
		| w == "next" -> assembleNext ws
		| w == "val"  -> assembleVal ws
		| w == "pack" -> assemblePack ws
		| otherwise   -> assembleDFU (w:ws)

assembleCFU :: [Word] -> Bits' 16
assembleCFU ws = case ws of
	"a"   :inst -> ass "a" "0"  inst
	c:cptr:inst -> ass c   cptr inst
	where
		ass a b c = do
			con <- cond a
			ptr <- rptr b
			tra <- tran c
			return $ con ++# ptr ++# tra

		tran :: [Word] -> Bits' 10
		tran ws = do
			name <- head' ws
			case name of
				"pushf"  -> return 0
				"pushq"  -> return 1
				"setval" -> do
					a1 <- head' (tail ws)
					a2 <- head' (tail $ tail ws)
					n1 <- readUnsigned d4 a1
					n2 <- readUnsigned d4 a2
					return $ 2 ++# n1 ++# n2
				"drop"   -> return 3
				_        -> Left "Unrecognized instruction"
		
		rptr :: Word -> Bits' 4
		rptr = readUnsigned d4
		
		cond :: Word -> Bits' 2
		cond "a"  = Right 0
		cond "z"  = Right 1
		cond "nz" = Right 2

assembleVal ws = do
	n <- single ws
	n <- readUnsigned d12 n
	return $ (0xD :: BitVector 4) ++# n 

assemblePack ws = do
	n <- single ws
	n <- readUnsigned d4 n
	return $ (0xC :: BitVector 4) ++# 0 ++# n

assembleNext ws = do
	n <- single ws
	n <- readUnsigned d12 n
	return $ (0xE :: BitVector 4) ++# n

assembleDFU ws = do
	n <- single ws
	i <- case n of
		"max"   -> Right 0
		"min"   -> Right 1
		"add"   -> Right 2
		"sub"   -> Right 3
		"mul"   -> Right 4
		"div"   -> Right 5
		"sqrt"  -> Right 6
		"abs"   -> Right 7
		"floor" -> Right 8
		_       -> Left "Unrecognized instruction"
	return $ (2 :: BitVector 2) ++# (i :: BitVector 6) ++# 0

readUnsigned s n = fmap pack $ read' s n 
	where
		read' :: KnownNat n => SNat n -> Word -> Either String (Unsigned n)
		read' _ n = case reads n of
			[(n', "")] -> Right n'
			[]         -> Left "Expected number"

isCond w = case filter (==w) ["a", "nz", "z"] of
	[] -> False
	_  -> True

single (l:[]) = Right l
single _      = Left "Wrong number of arguments"

head' (l:ls) = Right l
head' _      = Left "Not enough arguments"
