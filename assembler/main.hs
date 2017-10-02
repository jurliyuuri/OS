{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import System.IO(stderr, hPrint, hPutStr, hPutStrLn)
import System.Environment(getArgs)
import Control.Monad
import Data.List

import Parse
import Execute
import Linker

semicolonExtension :: String -> String
semicolonExtension = unlines . map (takeWhile (/=';')) . lines

fullExecute' :: [String] -> IO ()
fullExecute' strs = strs `getProgramAndApply` \program ->
 let (boolerh, logs) = execute program in do
  putStr "Logs: "
  print logs
  boolerh >>>= \(False, hardware) -> print hardware

getProgramAndApply :: [String] -> (Program -> IO ()) -> IO ()
strs `getProgramAndApply` f = mapM fullParse' strs >>>= \ps -> linker ps >>>= f 

(>>>=) :: (Show a) => Either a b -> (b -> IO ()) -> IO () 
Right b >>>= action = action b
Left a >>>= _ = hPrint stderr a

main :: IO ()
main = do
 args <- getArgs
 case args of 
  [] -> demo
  a
   | "-x" `elem` a -> interactive $ filter (/= "-x") a
   | otherwise -> main'' a

interactive :: [String] -> IO ()
interactive [] = hPutStrLn stderr "Give filepath."
interactive paths = do
 strs <- forM paths (fmap semicolonExtension . readFile)
 putStrLn $ "\npreparing step-by-step execution for " ++ intercalate ", " paths ++ ":\n"
 strs `getProgramAndApply` \program -> stepByStep (initialHardware initialAddress, program)

stepByStep :: (Hardware, Program) -> IO ()
stepByStep (hw, program) = do
 putStrLn "Press Enter to continue"
 _ <- getLine
 let (boolerh, logs) = unwrapWith (hw, program) (execOne(return True)) in do
  putStr "Logs: "
  print logs
  boolerh >>>= \(isContinuing, newHW) -> do
   print newHW
   if isContinuing 
    then stepByStep (newHW, program)
    else putStrLn "Execution correctly terminated."


main'' :: [FilePath] -> IO ()
main'' paths = do
 mapM_ parse' paths
 strs <- forM paths (fmap semicolonExtension . readFile)
 putStrLn $ "\nrunning " ++ intercalate ", " paths ++ ":\n"
 fullExecute' strs

main' :: FilePath -> IO ()
main' filepath = main'' [filepath]

parse' :: FilePath -> IO ()
parse' filepath = do
 str <- semicolonExtension <$> readFile filepath
 putStrLn $ "\nparsing " ++ filepath ++ ":\n"
 print $ fullParse' str


demo :: IO ()
demo = do
 main' "fib_non_recursive"
 main' "fib_recursive"
 main' "tarai"
 main' "quicksort"

