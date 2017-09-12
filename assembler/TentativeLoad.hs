module TentativeLoad
(toTentativeLoad
,TentativeLoad(..)
,initialAddress
) where

import Types
import Control.Monad.State
import Data.Char
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import System.Random

data TentativeLoad = TentativeLoad {
 tentativeAddressTable :: M.Map Word32 (Word32, Instruction), 
 labelTable :: M.Map Label Word32
 } deriving(Show, Eq, Ord)

toTentativeLoad :: [(Instruction, [Label])] -> TentativeLoad
toTentativeLoad arr = TentativeLoad {
 tentativeAddressTable = M.fromList raw1, 
 labelTable = M.fromList $ concatMap (\(bs,d) -> zip bs $ repeat d) raw2
 }
 where 
  (raw1, raw2) = unzip $ zipWith3 f ts (tail ts) arr
  ts = tentativeAddressList
  f :: Word32 -> Word32 -> (Instruction, [Label]) -> 
       ((Word32,(Word32, Instruction)),([Label],Word32))
  f addr next (ins, ls) = ((addr,(next,ins)),(ls,addr))

initialAddress :: Word32
initialAddress = 0x1482e8d4 -- just a random value with no meaning

tentativeAddressList :: [Word32]
tentativeAddressList = scanl (+) initialAddress rands
 where rands = randomRs (1,4) $ mkStdGen 0x90b1d666 -- random-like
