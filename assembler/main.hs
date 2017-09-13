import Parse
import Execute
import TentativeLoad
import System.IO(stderr, hPrint)
import Data.Word

--fullExecute :: String -> 
fullExecute :: String -> IO () --Either ParseError (Either RuntimeError Hardware)
fullExecute str = fullParse str >>>= \p -> execute (toTentativeLoad p) >>>= print

(>>>=) :: (Show a) => Either a b -> (b -> IO ()) -> IO () 
Right b >>>= action = action b
Left a >>>= _ = hPrint stderr a

main :: IO ()
main = do
 putStrLn "\nparsing fib_non_recursive:"
 print $ toTentativeLoad <$> fullParse program
 putStrLn "\nparsing fib_recursive:"
 print $ toTentativeLoad <$> fullParse program2
 putStrLn "\nparsing qsort:"
 print $ toTentativeLoad <$> fullParse program3
 putStrLn "\nrunning fib_non_recursive:"
 fullExecute program
 putStrLn "\nrunning fib_recursive:"
 fullExecute program2

push12AndCall name = "'c'i    nta f5 4    krz f5@ 12    nta f5 4    inj f5@ xx "++name++"     ata f5 8    krz xx f5@  "

program :: String
program = push12AndCall "fib1" ++ "'c'i  nll fib1  krz f0 f5 + 4 @  krz f1 0  krz f2 1  fi f0 0 clo  l' is  malkrz xx ka   nta f0 1  krz f3 f1  ata f3 f2  inj f1 f2 f3  krz xx is  krz f0 f1  l' ka  krz xx f5@"

program2 :: String
program2 = push12AndCall "fib2" ++ "'c'i  nll fib2  krz f0 f5+4@  fi f0 2 xylo  malkrz xx iska  krz f1 f0  nta f1 1  nta f5 4  krz f5@ f1  nta f5 4  inj f5@ xx fib2  ata f5 4  krz f1 f5@  krz f2 f0  nta f1 1  krz f5@ f2  nta f5 4  krz f5@ f1  nta f5 4  inj f5@ xx fib2  ata f5 4  krz f2 f5+4@  ata f5 8  ata f0 f2  krz xx f5@  l' iska"


--buggy quicksort
program3 :: String
program3 = let arr = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9]; addr = 2782070968 in "'c'i   krz f1 2782070968   " 
 ++ (arr >>= \x -> " krz f1@ "++show x++"   ata f1 4")
 ++ push addr ++ push 0 ++ push (fromIntegral (length arr) - 1)
 ++ " nta f5 4    inj f5@ xx ycax  ata f5 4    ata f5 12    krz xx f5@  "
 ++ " 'c'i  nll ycax  krz f2 f5+4@  krz f1 f5+8@  krz f0 f5+12@  fi f1 f2 xolo  malkrz xx lus  krz f3 f1  ata f3 1  fi f3 f5+4@ llo  l' panqa  malkrz xx fistir  krz f2 f5+8@  dro f2 2  krz f2 f0+f2@   dro f3 2  krz f5+4294967292@ f0+f3@  dto f3 2  fi f2 f5+4294967292@ xtlo  malkrz xx iska  ata f1 1  dro f1 2  dro f3 2  inj f0+f3@ f0+f1@ f0+f3@  dto f1 2  dto f3 2  ata f3 1  l' iska  krz xx panqa  krz f2 f5+8@  l' fistir  dro f1 2  dro f2 2  inj f0+f1@ f0+f2@ f0+f1@   dto f1 2  dto f2 2    nta f5 4  krz f5@ f1  nta f5 4  krz f5@ f5+20@  nta f5 4  krz f5@ f5+20@  nta f5 4  krz f1 f5+12@  nta f1 1  krz f5@ f1  nta f5 4  inj f5@ xx ycax  ata f5 8  krz f1 f5+8@  ata f1 1  krz f5@ f1  nta f5 4  krz f5@ f5+20@  nta f5 4  inj f5@ xx ycax  ata f5 20  krz xx f5@  l' lus"
push :: Word32 -> String
push a = " nta f5 4   krz f5@ " ++ show a


program4 :: String
program4 = " 'c'i" ++ push 5 ++ push 6 ++ push 0 ++ " 'i'c inj tarai xx f5@   ata 12 f5    krz f5@ xx " ++ "'i'c    nll tarai    krz f5+4@ f2    krz f5+8@ f1    krz f5+12@ f0    fi f0 f1 llo    malkrz fistir xx    krz f1 f0    krz f5@ xx        nll fistir    nta 4 f5   krz f0 f5@    nta 4 f5   krz f1 f5@    nta 4 f5   krz f2 f5@        nta 4 f5   krz f0 f5@   nta 1 f5@    nta 4 f5   krz f1 f5@    nta 4 f5   krz f2 f5@    nta 4 f5   inj tarai xx f5@   ata 16 f5    nta 4 f5   krz f0 f5@        nta 4 f5   krz f5+12@ f5@   nta 1 f5@    nta 4 f5   krz f5+12@ f5@    nta 4 f5   krz f5+24@ f5@    nta 4 f5   inj tarai xx f5@   ata 16 f5    nta 4 f5   krz f0 f5@        nta 4 f5   krz f5+12@ f5@   nta 1 f5@    nta 4 f5   krz f5+24@ f5@    nta 4 f5   krz f5+24@ f5@    nta 4 f5   inj tarai xx f5@   ata 16 f5    nta 4 f5   krz f0 f5@        nta 4 f5   inj tarai xx f5@   ata 16 f5        ata 12 f5    krz f5@ xx"



