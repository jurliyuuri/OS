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

type PageId = Int

linker' :: [ParsedFile] -> Either LinkError Program'
linker' pfs = case fromListNoDup $ zipWith assignInts pfs [1..] of
 Left _ -> Left $ LinkError "multiple files lack `kue`"
 Right dat -> case M.lookup 0 dat of 
  Nothing -> Left $ LinkError "all files have `kue`"
  _ -> do
   loadeds' <- M.traverseWithKey loadWithInt dat
   return $ Program' loadeds'

loadWithInt :: PageId -> ParsedFile -> Either LinkError (TentativeLoad, ([Label], [Label]))
loadWithInt n (ils, (kues, xoks)) = do
 loaded <- toTentativeLoad (initialAddress + (fromIntegral n)*maxSize) ils
 return (loaded, (kues, xoks))

assignInts :: ParsedFile -> PageId -> (PageId, ParsedFile)
assignInts a@(_, ([], _)) _ = (0, a) -- no kue means main
assignInts a n = (n, a)



data Program' = Program' {loads :: M.Map PageId (TentativeLoad, ([Label], [Label]))}

initialAddress :: Word32
initialAddress = 0x14830000 -- just a random value with no meaning

---- old code
data Program = Program TentativeLoad

linker :: [ParsedFile] -> Either LinkError Program
linker [(ils, ([], []))] = Program <$> toTentativeLoad initialAddress ils
linker _ = error "Linking multiple files is not yet implemented"

resolveLabel :: Word32 -> Program -> Label -> Maybe Word32
resolveLabel currentNX (Program ttl) label = let lt = labelTable ttl in M.lookup label lt

readNX :: Program -> Word32 -> Maybe (Word32, Instruction)
readNX (Program ttl) currentNX = M.lookup currentNX (tentativeAddressTable ttl)
