module Types
(Memory
,Register(..)
,Lvalue(..)
,Rvalue(..)
,Instruction(..)
,Cond(..)
,Label
,Word32
) where
import qualified Data.Map as M
import Data.Word
import Memory

data Register = F0 | F1 | F2 | F3 | F5 | XX deriving (Show, Eq, Ord)
data Lvalue = Re Register | RPlusNum Register Word32 | RPlusR Register Register deriving (Show, Eq, Ord)
data Rvalue = L Lvalue | Pure Word32 | Lab Label deriving (Show, Eq, Ord)
data Instruction = Krz Rvalue Lvalue | Ata Rvalue Lvalue | Nta Rvalue Lvalue | Ada Rvalue Lvalue | Ekc Rvalue Lvalue | Dal Rvalue Lvalue | MalKrz Rvalue Lvalue | Fi Rvalue Rvalue Cond | Inj Rvalue Lvalue Lvalue deriving (Show, Eq, Ord)  
data Cond = Xtlo | Xylo | Clo | Xolo | Llo deriving (Show, Eq, Ord)
type Label = String
