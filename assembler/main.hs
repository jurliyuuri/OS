import Parse
import Execute
import TentativeLoad
import System.IO(stderr, hPrint, hPutStr, hPutStrLn)
import System.Environment(getArgs)

semicolonExtension :: String -> String
semicolonExtension = unlines . map (takeWhile (/=';')) . lines

fullExecute :: String -> IO ()
fullExecute str = fullParse str >>>= \p -> 
 let (erh, logs) = execute (toTentativeLoad p) in do
  erh >>>= print
  putStr "Logs: "
  print logs

(>>>=) :: (Show a) => Either a b -> (b -> IO ()) -> IO () 
Right b >>>= action = action b
Left a >>>= _ = hPrint stderr a

main :: IO ()
main = do
 args <- getArgs
 case args of 
  [] -> foo
  a@(x:_)
   | "-x" `elem` a -> interactive $ filter (/= "-x") a
   | otherwise -> main' x

interactive :: [String] -> IO ()
interactive [] = hPutStrLn stderr "Give filepath."
interactive (filepath:_) = do
 str <- semicolonExtension <$> readFile filepath
 putStrLn $ "\npreparing step-by-step execution for " ++ filepath ++ ":\n"
 fullParse str >>>= \p -> do
  let loaded = toTentativeLoad p
  hPutStrLn stderr "Under construction"
  undefined

main' :: FilePath -> IO ()
main' filepath = do
 parse' filepath
 str <- semicolonExtension <$> readFile filepath
 putStrLn $ "\nrunning " ++ filepath ++ ":\n"
 fullExecute str

parse' :: FilePath -> IO ()
parse' filepath = do
 str <- semicolonExtension <$> readFile filepath
 putStrLn $ "\nparsing " ++ filepath ++ ":\n"
 print $ toTentativeLoad <$> fullParse str


foo :: IO ()
foo = do
 main' "fib_non_recursive"
 main' "fib_recursive"
 main' "tarai"
 main' "quicksort"

