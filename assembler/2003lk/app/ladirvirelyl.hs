{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import           Control.Monad
import           Data.List
-- import System.IO
import           System.Environment (getArgs)

import           CommonIO
-- import Parse
-- import Execute
-- import Linker
import           Messages
import           Types

main :: IO ()
main = do
  args <- getArgs
  main'' args

main'' :: [FilePath] -> IO ()
main'' paths = do
  mapM_ parse' paths
  strs <- forM paths (fmap semicolonExtension . readFile)
  putStrLn $
    show' English $
    NormalMessage $ "\ncompiling " ++ intercalate ", " paths ++ ":\n"
  fullCompile' strs

fullCompile' :: [String] -> IO ()
fullCompile' strs = undefined
