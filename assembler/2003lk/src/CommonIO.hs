{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module CommonIO
  ( semicolonExtension
  , parse'
  , (>>>=)
  ) where

import           System.IO

import           Messages
import           Parse
import           Types

semicolonExtension :: String -> String
semicolonExtension = unlines . map (takeWhile (/= ';')) . lines

parse' :: FilePath -> IO ()
parse' filepath = do
  str <- semicolonExtension <$> readFile filepath
  putStrLn $ show' English $ NormalMessage $ "\nparsing " ++ filepath ++ ":\n"
  fullParse' str >>>= print

(>>>=) :: (Message a) => Either a b -> (b -> IO ()) -> IO ()
Right b >>>= action = action b
Left a >>>= _ = hPutStrLn stderr (show' English a)
