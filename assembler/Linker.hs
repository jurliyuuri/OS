{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Linker
(linker
,Program
,initialAddress
,resolveLabel
,readNX
,readNX'
,linker'
) where
import Types
import Parse
import TentativeLoad
import qualified Data.Map as M
import Data.List
import Control.Monad
-- type ParsedFile = ([(Instruction, [Label])],([KueInfo],[Label]))

type PageId = Int

linker' :: [ParsedFile] -> Either LinkError Program'
linker' pfs = case fromListNoDup $ zipWith assignInts pfs [1..] of
 Left _ -> Left $ LinkError "multiple files lack `kue`"
 Right dat -> case M.lookup 0 dat of 
  Nothing -> Left $ LinkError "all files have `kue`"
  _ -> do
   loadeds' <- M.traverseWithKey loadWithInt dat
   sanitize loadeds'

sanitize :: M.Map PageId (TentativeLoad, ([Label], [Label])) -> Either LinkError Program'
sanitize foo = do
 return $ Program' foo

loadWithInt :: PageId -> ParsedFile -> Either LinkError (TentativeLoad, ([Label], [Label]))
loadWithInt n (ils, (kues, xoks)) = do
 loaded <- toTentativeLoad (initialAddress + (fromIntegral n)*maxSize) ils
 -- xoks must not conflict with internal label table
 let xokConflicts = filter (`M.member` labelTable loaded) xoks
 unless (null xokConflicts) $ Left $ LinkError $ 
   "conflict: cannot import label(s) `" ++ intercalate ", " (map unLabel xokConflicts) ++ 
   "` that is already defined in the file"
 return (loaded, (kues, xoks))

assignInts :: ParsedFile -> PageId -> (PageId, ParsedFile)
assignInts a@(_, ([], _)) _ = (0, a) -- no kue means main
assignInts a n = (n, a)

readNX' :: Program' -> Word32 -> Maybe (Word32, Instruction)
readNX' (Program' pages) currentNX = do
 (ttl, _) <- M.lookup (toPageId currentNX) pages
 M.lookup currentNX (tentativeAddressTable ttl)

resolveLabel' :: Word32 -> Program' -> Label -> Maybe Word32
resolveLabel' currentNX (Program' pages) label = do
 (ttl, _) <- M.lookup (toPageId currentNX) pages -- open the page
 undefined

toPageId :: Word32 -> PageId
toPageId addr = fromIntegral $ (addr - initialAddress) `div` maxSize

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
