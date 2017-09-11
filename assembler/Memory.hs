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
  f t i = ((fromIntegral t)::Word32) `shift` i

data Memory = Memory {unM :: M.Map Word32 Word8, garbages :: [(Word32,Word8)]} deriving(Show, Eq, Ord)

emptyM :: Memory
emptyM = Memory M.empty []


writeM :: Word32 -> Word32 -> State Memory ()
writeM addr dat = do
 let (a,b,c,d) = decompose dat
 Memory m gs <- get
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

