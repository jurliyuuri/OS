module Memory
(Memory()
,writeM
,readM
,emptyM
,unM
,runState,execState,evalState
) where
import qualified Data.Map as M
import Data.Word
import Control.Monad.State
import System.Random

data Memory = Memory {unM :: M.Map Word32 Word32, garbages :: [(Word32,Word32)]} deriving(Show, Eq, Ord)

emptyM :: Memory
emptyM = Memory M.empty []


writeM :: Word32 -> Word32 -> State Memory ()
writeM addr dat = do
 Memory m gs <- get
 put $ Memory (M.insert addr dat m) gs

readM :: Word32 -> State Memory Word32
readM addr = do
 Memory m gs <- get
 case M.lookup addr m of
  Just dat -> return dat
  Nothing -> do
   let garbage = fst . random . mkStdGen . fromIntegral $ addr -- make random-like data from addr info
   put $ Memory m ((addr,garbage):gs)
   writeM addr garbage
   return garbage