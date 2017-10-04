{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Linker
(linker
,Program
,initialAddress
,resolveLabel
,readNX
,linker'
) where
import Types
import Parse
import TentativeLoad
import qualified Data.Map as M
-- type ParsedFile = ([(Instruction, [Label])],([KueInfo],[Label]))


linker' :: [ParsedFile] -> Either LinkError Program'
linker' pfs = case fromListNoDup $ zipWith assignInts pfs [1..] of
 Left _ -> Left $ LinkError "multiple files lack `kue`"
 Right dat -> case M.lookup 0 dat of 
  Nothing -> Left $ LinkError "all files have `kue`"
  _ -> do
   loadeds' <- M.traverseWithKey loadWithInt dat
   let loadeds = map snd $ M.toAscList loadeds' -- [(TentativeLoad, ([Label], [Label])]                              
   undefined

loadWithInt :: Int -> ParsedFile -> Either LinkError (TentativeLoad, ([Label], [Label]))
loadWithInt n (ils, (kues, xoks)) = do
 loaded <- toTentativeLoad (initialAddress + (fromIntegral n)*maxSize) ils
 return (loaded, (kues, xoks))

assignInts :: ParsedFile -> Int -> (Int, ParsedFile)
assignInts a@(ils, ([], xoks)) n = (0, a) -- no kue means main
assignInts a n = (n, a)

maxSize :: Word32
maxSize = 65536


data Program' = Program' {loads :: [TentativeLoad]}

initialAddress :: Word32
initialAddress = 0x1482e8d4 -- just a random value with no meaning

---- old code
data Program = Program TentativeLoad

linker :: [ParsedFile] -> Either LinkError Program
linker [(ils, ([], []))] = Program <$> toTentativeLoad initialAddress ils
linker _ = error "Linking multiple files is not yet implemented"

resolveLabel :: Program -> Label -> Maybe Word32
resolveLabel (Program ttl) label = let lt = labelTable ttl in M.lookup label lt

readNX :: Program -> Word32 -> Maybe (Word32, Instruction)
readNX (Program ttl) currentNX = M.lookup currentNX (tentativeAddressTable ttl)
