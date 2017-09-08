import qualified Data.Map as M
import Data.Char
import Data.Word
import Control.Monad.State
import Data.Maybe

type Memory = M.Map Word32 Word32
data CPU = CPU{ f0 :: Word32, f1 :: Word32, f2 :: Word32, f3 :: Word32, f5 :: Word32, nx :: Word32, xx :: Word32, flag :: Bool, memory :: Memory} deriving (Show, Eq, Ord)
data Register = F0 | F1 | F2 | F3 | F5 | XX deriving (Show, Eq, Ord)
data Lvalue = Re Register | RPlusNum Register Word32 | RPlusR Register Register deriving (Show, Eq, Ord)
data Rvalue = L Lvalue | Pure Word32 | Lab Label deriving (Show, Eq, Ord)
data Instruction = Krz Rvalue Lvalue | Ata Rvalue Lvalue | Nta Rvalue Lvalue | MalKrz Rvalue Lvalue | Fi Rvalue Rvalue Cond | Inj Rvalue Lvalue Lvalue deriving (Show, Eq, Ord)  
data Cond = Xtlo | Xylo | Clo | Xolo | Llo deriving (Show, Eq, Ord)
type Label = String

rl :: String -> Maybe(Rvalue -> Lvalue -> Instruction)
rl "krz" = Just Krz
rl "ata" = Just Ata
rl "nta" = Just Nta
rl "malkrz" = Just MalKrz
rl _ = Nothing

parseCond :: String -> Maybe Cond
parseCond "xtlo" = Just Xtlo
parseCond "xylo" = Just Xylo
parseCond "clo"  = Just Clo
parseCond "xolo" = Just Xolo
parseCond "llo"  = Just Llo
parseCond _ = Nothing

main :: IO ()
main = do
 let ts = beautify . words $ map toLower program
 print $ toInstructions ts

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
 Right ils -> reverse . normalize . reverse $ ils

normalize :: [(Maybe a, [b])] -> [(a,[b])]
normalize [] = []
normalize ((Nothing,ls):(a,bs):ys) = normalize $ (a,ls++bs) : ys
normalize [(Nothing,_)] = error "l' must be preceded by an instruction"
normalize ((Just a,ls):ys) = (a,ls) : normalize ys

type Error = String

-- isCI :: Bool
toI :: [String] -> StateT Bool (Either Error) [(Maybe Instruction, [Label])]
toI [] = return []
toI ("'c'i" : xs) = put True >> toI xs
toI ("'i'c" : xs) = put True >> toI xs
toI ("fen" : xs) = do
 rest <- toI xs
 return $ (Just$Krz (L (Re F0)) (Re F0),[]) : rest
toI (str :x:y:zs)
 | isJust $ rl str = do
  isCI <- get
  i <- lift $ if isCI then parseR y else parseR x
  c <- lift $ if isCI then parseL x else parseL y
  rest <- toI zs
  return $ (Just$(fromJust $ rl str) i c,[]) : rest
toI ("fi":x:y:z:bs)
 | isJust $ parseCond z = do
  a <- lift $ parseR x
  b <- lift $ parseR y
  rest <- toI bs
  return $ (Just$Fi a b (fromJust $ parseCond z),[]) : rest
toI ("inj":x:y:z:bs) = do
 isCI <- get
 a <- lift $ if isCI then parseR z else parseR x
 b <- lift $ parseL y
 c <- lift $ if isCI then parseL x else parseL z
 rest <- toI bs
 return $ (Just$Inj a b c,[]) : rest
toI ("nll":x:ys) = do
 rest <- toI ys
 case rest of
  [] -> lift $ Left "nll must be followed by an instruction"
  ((Just a,b):xs) -> return $ (Just a,toLabel x:b):xs
  ((Nothing,_):_) -> lift $ Left "nll must not be followed by l'"
toI ("l'":x:ys) = do
 rest <- toI ys
 return $ (Nothing,[toLabel x]):rest
toI xs = error $ "Unparsable command sequence " ++ show xs
 
toLabel :: String -> Label
toLabel = id

parseRegister :: String -> Either Error Register
parseRegister "f0" = Right F0
parseRegister "f1" = Right F1
parseRegister "f2" = Right F2
parseRegister "f3" = Right F3
parseRegister "f5" = Right F5
parseRegister "xx" = Right XX
parseRegister _ = Left "no register"

-- data Rvalue = L Lvalue | Pure Word32 | Lab Label deriving (Show, Eq, Ord)

parseR :: String -> Either Error Rvalue
parseR str
 | isRight (parseL str) = return $ L (fromRight(parseL str))
 | all (`elem` "1234567890") str = return $ Pure $ read str
 | all isAlphaNum str = return $ Lab str
 | otherwise = Left $ "cannot parse `" ++ str ++ "` as a valid data"

isRight :: Either t1 t -> Bool
isRight (Right _) = True
isRight _ = False
fromRight :: Either t t1 -> t1
fromRight (Right a) = a
fromRight (Left _) = error "fromRight failed"

parseL :: String -> Either Error Lvalue
parseL s@[_,_] = Re <$> parseRegister s
parseL [a,b,'@'] = do{ re <- parseRegister [a,b]; return $ RPlusNum re 0}
parseL [a,b,'+',c,d,'@']
 | isRight (parseRegister [c,d]) = 
  let re2 = fromRight (parseRegister [c,d]) in
   do{ re <- parseRegister [a,b]; return $ RPlusR re re2}
parseL (a:b:'+':xs@(_:_)) = do
 re <- parseRegister [a,b]
 guard (last xs == '@')
 let num = read(init xs)
 return $ RPlusNum re num
parseL xs = Left $ "cannot parse `" ++ xs ++ "` as a valid place to put data"


program :: String
program = "'c'i  krz f0 f5 + 4 @  krz f1 0  krz f2 1  fi f0 0 clo  l' is  malkrz xx ka   nta f0 1  krz f3 f1  ata f3 f2  inj f1 f2 f3  krz xx is  krz f0 f1  l' ka  krz xx f5@"
