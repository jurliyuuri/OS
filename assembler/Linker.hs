{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Linker
(linker
,Program
) where
import Types
import Parse
import TentativeLoad

type Program = TentativeLoad


-- type ParsedFile = ([(Instruction, [Label])],([KueInfo],[Label]))
linker :: [ParsedFile] -> Either String Program
linker [(ils, ([], []))] = toTentativeLoad ils
linker _ = undefined