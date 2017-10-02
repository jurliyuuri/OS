{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Types
(Memory
,Register(..)
,Lvalue(..)
,Rvalue(..)
,Instruction(..)
,Cond(..)
,Label(), unLabel
,Word32
,ParseError(..)
,RuntimeError(..)
,LinkError(..)
,toFunc
,toLabel'
) where
import Data.Word
import Data.Char

import Memory

data Register = F0 | F1 | F2 | F3 | F5 | XX deriving (Show, Eq, Ord)
data Lvalue = Re Register | RPlusNum Register Word32 | RPlusR Register Register deriving (Show, Eq, Ord)
data Rvalue = L Lvalue | Pure Word32 | Lab Label deriving (Show, Eq, Ord)
data Instruction = TERMINATE | Krz Rvalue Lvalue | Ata Rvalue Lvalue | Nta Rvalue Lvalue | Ada Rvalue Lvalue | Ekc Rvalue Lvalue | Dal Rvalue Lvalue | Dto Rvalue Lvalue | Dro Rvalue Lvalue | MalKrz Rvalue Lvalue | Fi Rvalue Rvalue Cond | Inj Rvalue Lvalue Lvalue deriving (Show, Eq, Ord)  
data Cond = Xtlo | Xylo | Clo | Xolo | Llo | Niv | Xtlonys | Xylonys | Xolonys | Llonys deriving (Show, Eq, Ord)
newtype Label = Label{unLabel :: String} deriving(Show, Eq, Ord)

newtype ParseError = ParseError String deriving(Show, Eq, Ord)
newtype RuntimeError = RuntimeError String deriving(Show, Eq, Ord)
newtype LinkError = LinkError String deriving(Show, Eq, Ord)

toFunc :: Cond -> (Word32 -> Word32 -> Bool)
toFunc Xtlonys = (<=)
toFunc Xylonys = (<)
toFunc Clo = (==)
toFunc Xolonys = (>=)
toFunc Llonys = (>)
toFunc Niv = (/=)
toFunc Xtlo = lif (<=)
toFunc Xylo = lif (<) 
toFunc Xolo = lif (>=)
toFunc Llo  = lif (>) 

lif :: (Word32 -> Word32 -> t) -> Word32 -> Word32 -> t
lif f a b = f (a+0x80000000) (b+0x80000000)

toLabel' :: String -> Maybe Label
toLabel' str
 | all isDigit str = Nothing
 | str `elem` ["f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "xx"] = Nothing
 | all (`elem` "pFftcxkqhRzmnrljwbVvdsgXiyuoea0123456789'-_") str = Just(Label str)
 | otherwise = Nothing

