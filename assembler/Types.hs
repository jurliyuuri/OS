module Types
(Memory
,Register(..)
,Lvalue(..)
,Rvalue(..)
,Instruction(..)
,Cond(..)
,Label
,Word32
,ParseError(..)
,RuntimeError(..)
,toFunc
) where
import Data.Word
import Memory

data Register = F0 | F1 | F2 | F3 | F5 | XX deriving (Show, Eq, Ord)
data Lvalue = Re Register | RPlusNum Register Word32 | RPlusR Register Register deriving (Show, Eq, Ord)
data Rvalue = L Lvalue | Pure Word32 | Lab Label deriving (Show, Eq, Ord)
data Instruction = TERMINATE | Krz Rvalue Lvalue | Ata Rvalue Lvalue | Nta Rvalue Lvalue | Ada Rvalue Lvalue | Ekc Rvalue Lvalue | Dal Rvalue Lvalue | Dto Rvalue Lvalue | Dro Rvalue Lvalue | MalKrz Rvalue Lvalue | Fi Rvalue Rvalue Cond | Inj Rvalue Lvalue Lvalue deriving (Show, Eq, Ord)  
data Cond = Xtlo | Xylo | Clo | Xolo | Llo | Niv | Xtlonys | Xylonys | Xolonys | Llonys deriving (Show, Eq, Ord)
type Label = String

newtype ParseError = ParseError String deriving(Show, Eq, Ord)
newtype RuntimeError = RuntimeError String deriving(Show, Eq, Ord)

toFunc :: (Ord a) => Cond -> (a -> a -> Bool)
toFunc Xtlonys = (<=)
toFunc Xylonys = (<)
toFunc Clo = (==)
toFunc Xolonys = (>=)
toFunc Llonys = (>)
toFunc Niv = (/=)
