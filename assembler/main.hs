import qualified Data.Map as M
import Data.Char
import Data.Word
import Control.Monad.State
import Data.Maybe

type Memory = M.Map Word32 Word32
data CPU = CPU{ f0 :: Word32, f1 :: Word32, f2 :: Word32, f3 :: Word32, f5 :: Word32, nx :: Word32, xx :: Word32, flag :: Bool, memory :: Memory} deriving (Show, Eq, Ord)
data Register = F0 | F1 | F2 | F3 | F5 | XX deriving (Show, Eq, Ord)
data Lvalue = R Register | RPlusNum Register Word32 | RPlusR Register Register deriving (Show, Eq, Ord)
data Rvalue = L Lvalue | Pure Word32 deriving (Show, Eq, Ord)
data Instruction = Krz Rvalue Lvalue | Ata Rvalue Lvalue | Nta Rvalue Lvalue | MalKrz Rvalue Lvalue | Fi Rvalue Lvalue Cond | Inj Rvalue Lvalue Lvalue deriving (Show, Eq, Ord)  
data Cond = Xtlo | Xylo | Clo | Xolo | Llo deriving (Show, Eq, Ord)
type Label = String

rl :: String -> Maybe(Rvalue -> Lvalue -> Instruction)
rl "krz" = Just Krz
rl "ata" = Just Ata
rl "nta" = Just Nta
rl "malkrz" = Just MalKrz
rl _ = Nothing


main = do
 let ts = beautify . words $ map toLower program
 undefined

beautify :: [String] -> [String]
beautify (x:"+":y:zs) = beautify $ (x ++ "+" ++ y) : zs
beautify (x:"@":ys) = beautify $ (x++"@") : ys
beautify [_,"+"] = error "Unexpected + at the end of input"
beautify ("+":_) = error "Unexpected + at the beginning of input"
beautify ("@":_) = error "Unexpected @ at the beginning of input"
beautify (x:xs) = x:(beautify xs)
beautify [] = []

toInstructions :: [String] -> [(Instruction, [Label])]
toInstructions strs = case toI strs `evalStateT` False of
 Left err -> error err
 Right ils -> ils

type Error = String

-- isCI :: Bool
toI :: [String] -> StateT Bool (Either Error) [(Instruction, [Label])]
toI [] = return []
toI ("'c'i" : xs) = put True >> toI xs
toI ("'i'c" : xs) = put True >> toI xs
toI ("fen" : xs) = do
 rest <- toI xs
 return $ (Krz (L (R F0)) (R F0),[]) : rest
toI (str :x:y:zs)
 | isJust $ rl str = do
  isCI <- get
  i <- lift $ if isCI then parseR y else parseR x
  c <- lift $ if isCI then parseL x else parseL y
  rest <- toI zs
  return $ ((fromJust $ rl str) i c,[]) : rest
  

parseR :: String -> Either Error Rvalue
parseR = undefined 

parseL :: String -> Either Error Lvalue
parseL = undefined 


program :: String
program = "'c'i  krz f0 f5 + 4 @  krz f1 0  krz f2 1  fi f0 0 clo  l' is  malkrz xx ka   nta f0 1  krz f3 f1  ata f3 f2  inj f1 f2 f3  krz xx is  krz f0 f1  l' ka  krz xx f5@"
