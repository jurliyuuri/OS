import Parse
import Execute
import TentativeLoad
import System.IO(stderr, hPrint, hPutStr, hPutStrLn)
import System.Environment(getArgs)
import Control.Monad
import Data.List

semicolonExtension :: String -> String
semicolonExtension = unlines . map (takeWhile (/=';')) . lines

fullExecute :: String -> IO ()
fullExecute str = fullParse str >>>= \p -> toTentativeLoad p >>>= \loaded -> 
 let (boolerh, logs) = execute loaded in do
  putStr "Logs: "
  print logs
  boolerh >>>= \(False, hardware) -> print hardware

fullExecute' :: [String] -> IO ()
fullExecute' strs = do
 let ps = map fullParse' strs
 undefined

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
 fullParse str >>>= \p -> toTentativeLoad p >>>= \loaded -> 
  bar (initialHardware initialAddress, loaded)

bar :: (Hardware, TentativeLoad) -> IO ()
bar (hw, program) = do
 putStrLn "Press Enter to continue"
 _ <- getLine
 let (boolerh, logs) = unwrapWith (hw, program) (execOne(return True)) in do
  putStr "Logs: "
  print logs
  boolerh >>>= \(isContinuing, newHW) -> do
   print newHW
   if isContinuing 
    then bar (newHW, program)
    else putStrLn "Execution correctly terminated."


main'' :: [FilePath] -> IO ()
main'' paths = do
 mapM_ parse' paths
 strs <- forM paths (fmap semicolonExtension . readFile)
 putStrLn $ "\nrunning " ++ intercalate ", " paths ++ ":\n"
 fullExecute' strs

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

