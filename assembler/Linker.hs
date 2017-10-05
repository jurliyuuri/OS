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
   sanitizeKue loadeds'

sanitizeKue :: M.Map PageId (TentativeLoad, ([Label], [Label])) -> Either LinkError Program'
sanitizeKue foo = do
 let pidKues = M.toList $ fmap (\(_,(ks,_)) -> ks) foo
 let kuePid = concatMap (\(a,bs) -> zip bs $ repeat a) $ pidKues
 case fromListNoDup kuePid of
  Right dat -> return $ Program' foo dat
  Left labels -> Left $ LinkError $
   "conflict: different files export the same label(s) `" ++ intercalate ", " (map unLabel labels) ++ "“"

loadWithInt :: PageId -> ParsedFile -> Either LinkError (TentativeLoad, ([Label], [Label]))
loadWithInt n (ils, (kues, xoks)) = do
 loaded <- toTentativeLoad (initialAddress + (fromIntegral n)*maxSize) ils
 -- xoks must not conflict with internal label table
 let xokConflicts = filter (`M.member` labelTable loaded) xoks
 unless (null xokConflicts) $ Left $ LinkError $ 
   "conflict: cannot import label(s) `" ++ intercalate ", " (map unLabel xokConflicts) ++ 
   "` that is already defined in the file"
 -- kues must come from internal label table 
 let kueWithoutEvidence = filter (`M.notMember` labelTable loaded) kues
 unless (null kueWithoutEvidence) $ Left $ LinkError $
  "cannot export label(s) `" ++ intercalate ", " (map unLabel kueWithoutEvidence) ++
  "` that is not defined in the file"
 return (loaded, (kues, xoks))

assignInts :: ParsedFile -> PageId -> (PageId, ParsedFile)
assignInts a@(_, ([], _)) _ = (0, a) -- no kue means main
assignInts a n = (n, a)

readNX' :: Program' -> Word32 -> Maybe (Word32, Instruction)
readNX' (Program'{loads=pages}) currentNX = do
 (ttl, _) <- M.lookup (toPageId currentNX) pages
 M.lookup currentNX (tentativeAddressTable ttl)

resolveLabel' :: Word32 -> Program' -> Label -> Maybe Word32
resolveLabel' currentNX (Program'{loads=pages, kueTable=kt}) label = do
 (ttl, (_, xoks)) <- M.lookup (toPageId currentNX) pages -- open the page
 case M.lookup label (labelTable ttl) of -- first, search locally
  Just a -> return a
  Nothing
   | label `notElem` xoks -> Nothing
   | otherwise -> do
    pageId <- M.lookup label kt -- ask kueTable for the page
    (ttl', _) <- M.lookup pageId pages -- open the page
    M.lookup label (labelTable ttl')

toPageId :: Word32 -> PageId
toPageId addr = fromIntegral $ (addr - initialAddress) `div` maxSize

data Program' = Program' {
 loads :: M.Map PageId (TentativeLoad, ([Label], [Label])), 
 kueTable :: M.Map Label PageId}

initialAddress :: Word32
initialAddress = 0x14830000 -- just a random value with no meaning

type Program = Program'

linker :: [ParsedFile] -> Either LinkError Program
linker = linker'

resolveLabel :: Word32 -> Program -> Label -> Maybe Word32
resolveLabel = resolveLabel'

readNX :: Program -> Word32 -> Maybe (Word32, Instruction)
readNX = readNX'
