{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Linker
(linker
,Program
,initialAddress
) where
import Types
import Parse
import TentativeLoad

type Program = TentativeLoad


-- type ParsedFile = ([(Instruction, [Label])],([KueInfo],[Label]))
linker :: [ParsedFile] -> Either String Program
linker [(ils, ([], []))] = toTentativeLoad initialAddress ils
linker _ = error "Linking multiple files is not yet implemented"

initialAddress :: Word32
initialAddress = 0x1482e8d4 -- just a random value with no meaning
