{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module TentativeLoad
  ( toTentativeLoad
  , TentativeLoad(..)
  , maxSize
  ) where

import           Control.Monad
import           Data.List
import qualified Data.Map      as M
import           System.Random hiding (next)

import           Types

maxSize :: Word32
maxSize = 65536

data TentativeLoad = TentativeLoad
  { tentativeAddressTable :: M.Map Word32 (Word32, Instruction)
  , labelTable            :: M.Map Label Word32
  } deriving (Show, Eq, Ord)

toTentativeLoad ::
     Word32 -> [(Instruction, [Label])] -> Either LinkError TentativeLoad
toTentativeLoad initAddress arr = do
  let list = concatMap (\(bs, d) -> zip bs $ repeat d) raw2
  unless (null arr) $
    let (_, (finalAddress, _)) = last raw1
     in when (finalAddress >= initAddress + maxSize) $
        Left $
        LinkError
          (Eng "size limit of a single file was exceeded")
          (Lpa
             "panqa'd chertifen xerfo es sach'arrefoien fai jurkenel'd snojostan")
  case fromListNoDup list of
    Right ltable ->
      return
        TentativeLoad
          {tentativeAddressTable = M.fromList raw1, labelTable = ltable}
    Left labels ->
      let info = intercalate ", " (map unLabel labels)
       in Left $
          LinkError
            (Eng $ "duplicate local label(s): " ++ info)
            (Lpa $ "xanalen firsykaloa'st sliejseso: " ++ info)
  where
    (raw1, raw2) = unzip $ zipWith3 f ts (tail ts) arr
    ts = tentativeAddressList initAddress
    f :: Word32
      -> Word32
      -> (Instruction, [Label])
      -> ((Word32, (Word32, Instruction)), ([Label], Word32))
    f addr next (ins, ls) = ((addr, (next, ins)), (ls, addr))

tentativeAddressList :: Word32 -> [Word32]
tentativeAddressList initAddress = scanl (+) initAddress rands
  where
    rands = randomRs (1, 4) $ mkStdGen 0x90b1d666 -- random-like
