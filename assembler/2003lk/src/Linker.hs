{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Linker
  ( Program
  , initialAddress
  , resolveLabel
  , readNX
  , linker
  ) where

import           Control.Monad
import           Data.List
import qualified Data.Map      as M

import           Parse
import           TentativeLoad
import           Types

-- type ParsedFile = ([(Instruction, [Label])],Kues_Xoks)
type PageId = Int

linker :: [ParsedFile] -> Either LinkError Program
linker pfs =
  case fromListNoDup $ zipWith assignInts [1 ..] pfs of
    Left _ ->
      Left $
      LinkError
        (Eng "multiple files lack `kue`")
        (Lpa "chertifess mol niv mels `kue`")
    Right dat ->
      case M.lookup 0 dat of
        Nothing ->
          Left $
          LinkError (Eng "all files have `kue`") (Lpa "als chertif laxn `kue`")
        _ -> do
          loadeds' <- M.traverseWithKey loadWithInt dat
          sanitizeKue loadeds'

sanitizeKue ::
     M.Map PageId (TentativeLoad, Kues_Xoks) -> Either LinkError Program
sanitizeKue foo = do
  let pidKues = M.toList $ fmap (\(_, (ks, _)) -> ks) foo
  let kuePid = concatMap (\(a, bs) -> zip bs $ repeat a) pidKues
  case fromListNoDup kuePid of
    Right dat -> return $ Program foo dat
    Left labels ->
      let info = "`" ++ intercalate ", " (map unLabel labels) ++ "`"
       in Left $
          LinkError
            (Eng $ "conflict: different files export the same label(s) " ++ info)
            (Lpa $
             "sliejseso: cuturlo eustira'd chertifess feat daliu'd firsykaloa " ++
             info)

loadWithInt ::
     PageId -> ParsedFile -> Either LinkError (TentativeLoad, Kues_Xoks)
loadWithInt n (ils, (kues, xoks)) = do
  loaded <- toTentativeLoad (initialAddress + fromIntegral n * maxSize) ils
 -- xoks must not conflict with internal label table
  let xokConflicts = filter (`M.member` labelTable loaded) xoks
  let info = intercalate ", " (map unLabel xokConflicts)
  unless (null xokConflicts) $
    Left $
    LinkError
      (Eng $
       "conflict: cannot import label(s) `" ++
       info ++ "` that is already defined in the file")
      (Lpa $
       "sliejseso: elx mouteo niv cene firsykaloa `" ++
       info ++ "`. la lex veles snojo xelvinj fal chertifestan.")
 -- kues must come from internal label table
  let kueWithoutEvidence = filter (`M.notMember` labelTable loaded) kues
  let info2 = intercalate ", " (map unLabel kueWithoutEvidence)
  unless (null kueWithoutEvidence) $
    Left $
    LinkError
      (Eng $
       "cannot export label(s) `" ++
       info2 ++ "` that is not defined in the file")
      (Lpa $
       "cene niv cuturl firsykaloa `" ++
       info2 ++ "`. la lex niv veles snojo fal chertifestan.")
  return (loaded, (kues, xoks))

assignInts :: PageId -> ParsedFile -> (PageId, ParsedFile)
assignInts _ a@(_, ([], _)) = (0, a) -- no kue means main
assignInts n a              = (n, a)

readNX :: Program -> Word32 -> Maybe (Word32, Instruction)
readNX Program {loads = pages} currentNX = do
  (ttl, _) <- M.lookup (toPageId currentNX) pages
  M.lookup currentNX (tentativeAddressTable ttl)

resolveLabel :: Word32 -> Program -> Label -> Maybe Word32
resolveLabel currentNX Program {loads = pages, kueTable = kt} label = do
  (ttl, (_, xoks)) <- M.lookup (toPageId currentNX) pages -- open the page
  case M.lookup label (labelTable ttl) -- first, search locally
        of
    Just a -> return a
    Nothing
      | label `notElem` xoks -> Nothing
      | otherwise -> do
        pageId <- M.lookup label kt -- ask kueTable for the page
        (ttl', _) <- M.lookup pageId pages -- open the page
        M.lookup label (labelTable ttl')

toPageId :: Word32 -> PageId
toPageId addr = fromIntegral $ (addr - initialAddress) `div` maxSize

data Program = Program
  { loads    :: M.Map PageId (TentativeLoad, ([Label], [Label]))
  , kueTable :: M.Map Label PageId
  }

initialAddress :: Word32
initialAddress = 0x14830000 -- just a random value with no meaning
