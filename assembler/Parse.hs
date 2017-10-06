{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Parse
(fullParse
,fullParse'
,ParsedFile
) where
import Control.Monad.State
import Data.Char(isDigit)
import Data.Maybe

import Types

type Error = ParseError
type ParsedFile = ([(Instruction, [Label])],([KueInfo],[Label]))

left :: String -> Either ParseError b
left = Left . ParseError

fullParse :: String -> Either Error [(Instruction, [Label])]
fullParse = fmap fst . fullParse'

fullParse' :: String -> Either Error ParsedFile
fullParse' str = do
 let ts = words $ concatMap plusAt str
 toInstructions <=< beautify $ ts
  where
   plusAt '@' = " @ "
   plusAt '+' = " + "
   plusAt a   = [a]


rl :: String -> Maybe(Rvalue -> Lvalue -> Instruction)
rl "krz" = Just Krz
rl "ata" = Just Ata
rl "nta" = Just Nta
rl "ada" = Just Ada
rl "ekc" = Just Ekc
rl "dal" = Just Dal
rl "dto" = Just Dto
rl "dtosna" = Just Dtosna
rl "dro" = Just Dro
rl "malkrz" = Just MalKrz
rl _ = Nothing

parseCond :: String -> Maybe Cond
parseCond "xtlo" = Just Xtlo
parseCond "xylo" = Just Xylo
parseCond "clo"  = Just Clo
parseCond "xolo" = Just Xolo
parseCond "llo"  = Just Llo
parseCond "niv"  = Just Niv
parseCond "xtlonys" = Just Xtlonys
parseCond "xylonys" = Just Xylonys
parseCond "xolonys" = Just Xolonys
parseCond "llonys"  = Just Llonys
parseCond _ = Nothing


beautify :: [String] -> Either Error [String]
beautify (x:"+":y:zs) = beautify $ (x ++ "+" ++ y) : zs
beautify (x:"@":ys) = beautify $ (x++"@") : ys
beautify [_,"+"] = left "Unexpected + at the end of input"
beautify ("+":_) = left "Unexpected + at the beginning of input"
beautify ("@":_) = left "Unexpected @ at the beginning of input"
beautify (x:xs) = (x:) <$> beautify xs
beautify [] = return []

toInstructions :: [String] -> Either Error ParsedFile
toInstructions strs = do
  (ils, P{kueList=kl,xokList=xl}) <- toI strs `runStateT` P{isCI=False, kueList=[], xokList=[]}
  ils' <- fmap reverse . normalize . reverse $ ils
  return (ils', (kl, xl))

normalize :: [(Maybe a, [b])] -> Either Error [(a,[b])]
normalize [] = return []
normalize ((Nothing,ls):(a,bs):ys) = normalize $ (a,ls++bs) : ys
normalize [(Nothing,_)] = left "l' must be preceded by an instruction"
normalize ((Just a,ls):ys) = ((a,ls) :) <$> normalize ys

type KueInfo = Label
data ParserStat = P {isCI :: Bool, kueList :: [Label], xokList :: [Label]} deriving (Show, Eq, Ord)

toI :: [String] -> StateT ParserStat (Either Error) [(Maybe Instruction, [Label])]
toI [] = return []
toI ("'c'i" : xs) = modify (\x -> x{isCI=True}) >> toI xs
toI ("'i'c" : xs) = modify (\x -> x{isCI=False}) >> toI xs
toI ("fen" : xs) = do
 rest <- toI xs
 return $ (Just$Krz (L (Re F0)) (Re F0),[]) : rest
toI ("nac":x:xs) = do
 a <- lift $ parseL x
 rest <- toI xs
 return $ (Just$Dal (Pure 0) a,[]): rest
toI (str :x:y:zs)
 | isJust $ rl str = do
  isCI_ <- isCI <$> get
  i <- lift $ if isCI_ then parseR y else parseR x
  c <- lift $ if isCI_ then parseL x else parseL y
  rest <- toI zs
  return $ (Just$(fromJust $ rl str) i c,[]) : rest
toI ("fi":x:y:z:bs)
 | isJust $ parseCond z = do
  a <- lift $ parseR x
  b <- lift $ parseR y
  rest <- toI bs
  return $ (Just$Fi a b (fromJust $ parseCond z),[]) : rest
toI ("inj":x:y:z:bs) = do
 isCI_ <- isCI <$> get
 a <- lift $ if isCI_ then parseR z else parseR x
 b <- lift $ parseL y
 c <- lift $ if isCI_ then parseL x else parseL z
 rest <- toI bs
 return $ (Just$Inj a b c,[]) : rest
toI ("nll":x:ys) = do
 rest <- toI ys
 case rest of
  [] -> lift $ left "nll must be followed by an instruction"
  ((Just a,b):xs) -> do
   label <- getLabelFrom x
   return $ (Just a,label:b):xs
  ((Nothing,_):_) -> lift $ left "nll must not be followed by l'"
toI ("l'":x:ys) = do
 rest <- toI ys
 label <- getLabelFrom x
 return $ (Nothing,[label]):rest
toI ("kue":x:ys) = do
 label <- getLabelFrom x
 modify (\u -> u{kueList = label:kueList u}) >> toI ys
toI ("xok":x:ys) = do
 label <- getLabelFrom x
 modify (\u -> u{xokList = label:xokList u}) >> toI ys
toI xs = lift $ left $ "Unparsable command sequence " ++ show xs
 
getLabelFrom :: String -> StateT ParserStat (Either Error) Label
getLabelFrom x = case toLabel' x of
 Nothing -> lift $ left $ "`" ++ x ++ "` cannot be used as a valid label"
 Just label -> return label

parseRegister :: String -> Either Error Register
parseRegister "f0" = Right F0
parseRegister "f1" = Right F1
parseRegister "f2" = Right F2
parseRegister "f3" = Right F3
parseRegister "f5" = Right F5
parseRegister "xx" = Right XX
parseRegister _ = left "no register"


parseR :: String -> Either Error Rvalue
parseR str
 | isRight (parseL str) = return $ L (fromRight(parseL str))
 | all isDigit str = return $ Pure $ read str
 | isJust (toLabel' str) = return $ Lab (fromJust(toLabel' str))
 | otherwise = left $ "cannot parse `" ++ str ++ "` as a valid data"

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
parseL (a:b:'+':xs@(_:_))
 | last xs == '@' && all isDigit (init xs) = do
 re <- parseRegister [a,b]
 let num = read(init xs)
 return $ RPlusNum re num
parseL xs = left $ "cannot parse `" ++ xs ++ "` as a valid place to put data"


