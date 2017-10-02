{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Memory
(Memory()
,writeM
,readM
,emptyM
,readByte
,writeByte
,unM
,runState,execState,evalState
) where
import qualified Data.Map as M
import Data.Word
import Control.Monad.State
import System.Random
import Data.Bits


decompose :: Word32 -> (Word8, Word8, Word8, Word8)
decompose a = 
 (fromIntegral (a `shift` (-24))
 ,fromIntegral (a `shift` (-16))
 ,fromIntegral (a `shift` (- 8))
 ,fromIntegral a)

compose :: (Word8, Word8, Word8, Word8) -> Word32
compose (a,b,c,d) = f a 24 + f b 16 + f c 8 + f d 0
 where
  f :: Word8 -> Int -> Word32
  f t i = (fromIntegral t::Word32) `shift` i

data Memory = Memory {unM :: M.Map Word32 Word8, garbages :: [(Word32,Word8)]} deriving(Eq, Ord)

emptyM :: Memory
emptyM = Memory M.empty []




writeM :: Word32 -> Word32 -> State Memory ()
writeM addr dat = do
 let (a,b,c,d) = decompose dat
 writeByte  addr    a
 writeByte (addr+1) b
 writeByte (addr+2) c
 writeByte (addr+3) d

writeByte :: Word32 -> Word8 -> State Memory ()
writeByte addr dat = do
 Memory m gs <- get
 put $ Memory (M.insert addr dat m) gs 

readM :: Word32 -> State Memory Word32
readM addr = do
 a <- readByte  addr
 b <- readByte (addr + 1)
 c <- readByte (addr + 2)
 d <- readByte (addr + 3)
 return $ compose (a,b,c,d)


readByte :: Word32 -> State Memory Word8
readByte addr = do
 Memory m gs <- get
 case M.lookup addr m of
  Just dat -> return dat
  Nothing -> do
   let garbage = fst . random . mkStdGen . fromIntegral $ addr -- make random-like data from addr info
   put $ Memory m ((addr,garbage):gs)
   writeByte addr garbage
   return garbage



instance Show Memory where
 show Memory{unM = a, garbages = b} = 
  "\nMemory: \n" ++ toStr (to32 a) ++ 
  "\nGarbages: \n" ++ toStr (to32(M.fromList b))

type Foo = (Maybe Word8, Maybe Word8, Maybe Word8, Maybe Word8)

toStr :: M.Map Word32 (Maybe Word32) -> String
toStr = unlines . map foobar . M.toList
 where
  foobar (a,b) = ('\t':) $ show a ++ ": " ++ case b of {Just q -> show q; Nothing -> "*"}

to32 :: M.Map Word32 Word8 -> M.Map Word32 (Maybe Word32)
to32 = fmap baz . M.fromListWith foo . map bar . M.toList
 where
  bar :: (Word32, Word8) -> (Word32, Foo)
  bar (w32, w8)
   | w32 `mod` 4 == 0 = (w32', (Just w8, Nothing, Nothing, Nothing))
   | w32 `mod` 4 == 1 = (w32', (Nothing, Just w8, Nothing, Nothing))
   | w32 `mod` 4 == 2 = (w32', (Nothing, Nothing, Just w8, Nothing))
   | otherwise        = (w32', (Nothing, Nothing, Nothing, Just w8))
   where w32' = (w32 `div` 4)*4
  foo :: Foo -> Foo -> Foo
  foo (a,b,c,d) (w,x,y,z) = (m a w, m b x, m c y, m d z)
  m :: Maybe a -> Maybe a -> Maybe a
  m Nothing b = b
  m a _ = a
  baz :: Foo -> Maybe Word32
  baz (a,b,c,d) = do
   w <- a
   x <- b
   y <- c
   z <- d
   return $ compose (w,x,y,z)

