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

type Program = TentativeLoad


-- type ParsedFile = ([(Instruction, [Label])],([KueInfo],[Label]))
linker :: [ParsedFile] -> Either String Program
linker [(ils, ([], []))] = toTentativeLoad initialAddress ils
linker _ = error "Linking multiple files is not yet implemented"

initialAddress :: Word32
initialAddress = 0x1482e8d4 -- just a random value with no meaning

resolveLabel' :: Program -> Label -> Maybe Word32
resolveLabel' program label = let lt = labelTable program in M.lookup label lt

readNX :: Program -> Word32 -> Maybe (Word32, Instruction)
readNX program currentNX = M.lookup currentNX (tentativeAddressTable program)
