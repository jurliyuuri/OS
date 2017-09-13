import Parse
import Execute
import TentativeLoad
import System.IO(stderr, hPrint)
import System.Environment(getArgs)

--fullExecute :: String -> 
fullExecute :: String -> IO () --Either ParseError (Either RuntimeError Hardware)
fullExecute str = fullParse str >>>= \p -> execute (toTentativeLoad p) >>>= print

(>>>=) :: (Show a) => Either a b -> (b -> IO ()) -> IO () 
Right b >>>= action = action b
Left a >>>= _ = hPrint stderr a

main :: IO ()
main = do
 args <- getArgs
 case args of 
  [] -> foo
  (x:_) -> main' x

main' :: FilePath -> IO ()
main' filepath = do
 parse' filepath
 str <- readFile filepath
 putStrLn $ "\nrunning " ++ filepath ++ ":\n"
 fullExecute str

parse' :: FilePath -> IO ()
parse' filepath = do
 str <- readFile filepath
 putStrLn $ "\nparsing " ++ filepath ++ ":\n"
 print $ toTentativeLoad <$> fullParse str


foo :: IO ()
foo = do
 main' "fib_non_recursive"
 main' "fib_recursive"
 parse' "buggy_quicksort"
 parse' "buggy_tarai"

