import Parse
import Execute
import TentativeLoad
import System.IO(stderr, hPrint, hPutStr, hPutStrLn)
import System.Environment(getArgs)

semicolonExtension :: String -> String
semicolonExtension = unlines . map (takeWhile (/=';')) . lines

fullExecute :: String -> IO ()
fullExecute str = fullParse str >>>= \p -> 
 let (boolerh, logs) = execute (toTentativeLoad p) in do
  putStr "Logs: "
  print logs
  boolerh >>>= \(False, hardware) -> print hardware

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
  bar (initialHardware initialAddress, loaded)

bar :: (Hardware, TentativeLoad) -> IO ()
bar (hw, program) = do
 putStrLn "Press Enter to continue"
 _ <- getLine
 let (boolerh, logs) = unwrapWith (hw, program) (execOne(return True)) in do
  putStr "Logs: "
  print logs
  boolerh >>>= \(isTerminated, newHW) -> do
   print newHW
   if isTerminated 
    then putStrLn "Execution correctly terminated."
    else bar (newHW, program)



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

