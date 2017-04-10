{-# LANGUAGE MultiWayIf #-}

module Inst 
		(Bits', Line, Word, cleanupCode, assembleInst) 
	where

import CLaSH.Prelude hiding (tail, Word, lines, zip, map)
import Data.List
import Prelude hiding (Word)

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
