{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import           Control.Monad
import           Data.List
import           System.Environment (getArgs)
import           System.IO

import           CommonIO
import           Execute
import           Linker
import           Messages
import           Parse
import           Types

fullExecute' :: [String] -> IO ()
fullExecute' strs =
  strs `getProgramAndApply` \program ->
    let (boolerh, logs) = execute program
     in do putStr $ show' English $ NormalMessage "Logs: "
           print logs
           boolerh >>>= \(False, hardware) -> print hardware

getProgramAndApply :: [String] -> (Program -> IO ()) -> IO ()
strs `getProgramAndApply` f = mapM fullParse' strs >>>= \ps -> linker ps >>>= f

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> demo
    a
      | "-x" `elem` a -> interactive $ filter (/= "-x") a
      | otherwise -> main'' a

interactive :: [String] -> IO ()
interactive [] =
  hPutStrLn stderr $ show' English $ NormalMessage "Give filepath."
interactive paths = do
  strs <- forM paths (fmap semicolonExtension . readFile)
  putStrLn $
    show' English $
    NormalMessage $
    "\npreparing step-by-step execution for " ++ intercalate ", " paths ++ ":\n"
  strs `getProgramAndApply` \program ->
    stepByStep (initialHardware initialAddress, program)

stepByStep :: (Hardware, Program) -> IO ()
stepByStep (hw, program) = do
  putStrLn $ show' English $ NormalMessage "Press Enter to continue"
  _ <- getLine
  let (boolerh, logs) = unwrapWith (hw, program) (execOne (return True))
   in do putStr $ show' English $ NormalMessage "Logs: "
         print logs
         boolerh >>>= \(isContinuing, newHW) -> do
           print newHW
           if isContinuing
             then stepByStep (newHW, program)
             else putStrLn $
                  show' English $
                  NormalMessage "Execution correctly terminated."

main'' :: [FilePath] -> IO ()
main'' paths = do
  mapM_ parse' paths
  strs <- forM paths (fmap semicolonExtension . readFile)
  putStrLn $
    show' English $
    NormalMessage $ "\nrunning " ++ intercalate ", " paths ++ ":\n"
  fullExecute' strs

demo :: IO ()
demo =
  mapM_
    main''
    [ ["fib_non_recursive"]
    , ["fib_recursive"]
    , ["fib_lib", "fib_main"]
    , ["tarai"]
    , ["quicksort"]
    ]
