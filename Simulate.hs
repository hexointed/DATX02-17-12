module Simulate where
--import CLaSH.Prelude
--import CLaSH.Prelude.Explicit
import Base
import Framebuffer
import GPU
import qualified Prelude as P
import System.Console.ANSI hiding (Color)
import Graphics.Gloss as G
import Graphics.Gloss.Data.ViewPort
import qualified Data.ByteString as B
import CLaSH.Class.BitPack

type NumPixels = 512
type SimIndex = Unsigned (Log 2 NumPixels)

showFrame = 
	fmap show' $
	mealy simDisplay (repeat (0,0,0), 0) $
	discardFirst $
	snd $ fb (fmap resize incr :: Signal (Unsigned 32))
	where
		show' vs = 
			('\n':)$
			foldr (\a b -> a P.++ '\n':b) "" $
			fmap (foldr (:) "") $ 
			unconcat d32 $
			fmap showPixel vs 
		
		showPixel (0,0,0) = ' '
		showPixel _       = '\x2588'

showFrameRGB = 
	mealy simDisplay (repeat (0,0,0), 0) $
	discardFirst $
	snd $ fb (fmap resize inc :: Signal (Unsigned 32))


simDisplay :: (Vec NumPixels Pixel, SimIndex) -> (Bool, Pixel, SimIndex) ->
	((Vec NumPixels Pixel, SimIndex), Vec NumPixels Pixel)

simDisplay (disp, i') (r, p, i) = case r of
	False -> ((disp, i), disp)
	True -> ((disp', i), disp')
		where
			disp' = replace i' p disp

discardFirst p = 
	register (False, (0,0,0), 0) $ 
	signal ((,,) True) <*> p <*> incr

incr :: Signal SimIndex
incr = mealy (\i _ -> (i+1, i) ) 0 (pure 0)

simGPU = 
	sequence $ 
	fmap putStr  ( 
	P.concatMap (P.take 1) . P.iterate (P.drop 1000) $ 
	sample showFrame )

simGPUclr = 
	sequence $ fmap ( (clearScreen >>) .  putStr)  ( 
	P.concatMap (P.take 1) . P.iterate (P.drop 150) $ 
	sample showFrame )


-- 80 ska bort , multplikation skall inte ske. är för debug.
picAtTime time = bm  --picAtTime time = Pictures [Blank, bm]  
	where 
	bm = bitmapOfByteString 16 16 (BitmapFormat TopToBottom PxRGBA) pxStr False
		where 
		pxStr = B.pack $ foldr (\x a -> (fromInteger.toInteger $ fst3 x) : (fromInteger.toInteger $ snd3 x) : (fromInteger.toInteger $ thd3 x):255:a) [] (simPixels time)
			where 
			fst3 (x,_,_) = x
			snd3 (_,x,_) = x
			thd3 (_,_,x) = x

--denna ligger i global scope för att kunna användas i debugging -- P.drop 8 kan ändras
simPixels time = P.head $ P.drop (P.round time) ( P.concatMap (P.take 1) . P.iterate (P.drop 8) $ sample showFrameRGB )

main = G.simulate pixBufWindow white (60::Int) 0.0 picAtTime update   
	where 
	update _ t st = st + 1
	pixBufWindow = InWindow "GPU" (200, 200) (10, 10)
