{-# LANGUAGE NoMonomorphismRestriction #-}

import System.Environment

avgFile file = do
	contents <- readFile file 
	putStrLn $ show $ averageS contents
	return ()

averageS :: String -> Double
averageS = average . map read . words

average :: [Double] -> Double
average xs = sum xs / length xs
	where
		length (x:xs) = 1 + length xs
		length []     = 0
