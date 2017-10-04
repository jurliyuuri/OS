{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Linker
(linker
,Program
,initialAddress
,resolveLabel'
,readNX
) where
import Types
import Parse
import TentativeLoad
import qualified Data.Map as M

data Program = Program TentativeLoad

linker' :: [ParsedFile] -> Either String Program'
linker' pfs = case fromListNoDup $ zipWith f pfs [1..] of
 Left _ -> Left "multiple files lack `kue`"
 Right dat -> case M.lookup 0 dat of 
  Nothing -> Left "all files have `kue`"
  _ -> linker2 dat

linker2 :: M.Map Word32 ParsedFile -> c
linker2 dat = let k = M.mapWithKey g dat in undefined

g :: Word32 -> ParsedFile -> (Either LinkError TentativeLoad, ([Label], [Label]))
g n (ils, (kues, xoks)) = (toTentativeLoad (initialAddress + n*maxSize) ils, (kues, xoks))

f :: ParsedFile -> Word32 -> (Word32, ParsedFile)
f a@(ils, ([], xoks)) n = (0, a) -- no kue means main
f a n = (n, a)

maxSize :: Word32
maxSize = 65536


data Program' = Program' {loads :: [TentativeLoad]}

-- type ParsedFile = ([(Instruction, [Label])],([KueInfo],[Label]))
linker :: [ParsedFile] -> Either LinkError Program
linker [(ils, ([], []))] = Program <$> toTentativeLoad initialAddress ils
linker _ = error "Linking multiple files is not yet implemented"

initialAddress :: Word32
initialAddress = 0x1482e8d4 -- just a random value with no meaning

resolveLabel' :: Program -> Label -> Maybe Word32
resolveLabel' (Program ttl) label = let lt = labelTable ttl in M.lookup label lt

readNX :: Program -> Word32 -> Maybe (Word32, Instruction)
readNX (Program ttl) currentNX = M.lookup currentNX (tentativeAddressTable ttl)
