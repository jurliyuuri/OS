import Parse
import Execute
import TentativeLoad
import Types

--fullExecute :: String -> 
fullExecute :: String -> Either ParseError (Either RuntimeError Hardware)
fullExecute str = execute <$> (toTentativeLoad <$> fullParse str)

main :: IO ()
main = do
 putStrLn "\nfib_non_recursive:"
 print $ toTentativeLoad <$> fullParse program
 putStrLn "\nfib_recursive:"
 print $ toTentativeLoad <$> fullParse program2
 putStrLn "\nqsort:"
 print $ toTentativeLoad <$> fullParse program3

program :: String
program = "'c'i  krz f0 f5 + 4 @  krz f1 0  krz f2 1  fi f0 0 clo  l' is  malkrz xx ka   nta f0 1  krz f3 f1  ata f3 f2  inj f1 f2 f3  krz xx is  krz f0 f1  l' ka  krz xx f5@"

program2 :: String
program2 = "'c'i  nll fib2  krz f0 f5+4@  fi f0 2 xylo  malkrz xx iska  krz f1 f0  nta f1 1  nta f5 4  krz f5@ f1  nta f5 4  inj f5@ xx fib2  ata f5 4  krz f1 f5@  krz f2 f0  nta f1 1  krz f5@ f2  nta f5 4  krz f5@ f1  nta f5 4  inj f5@ xx fib2  ata f5 4  krz f2 f5+4@  ata f5 8  ata f0 f2  krz xx f5@  l' iska"

program3 :: String
program3 = "'c'i  nll ycax  krz f2 f5+4@  krz f1 f5+8@  krz f0 f5+12@  fi f1 f2 xolo  malkrz xx lus  krz f3 f1  ata f3 1  fi f3 f5+4@ llo  l' panqa  malkrz xx fistir  krz f2 f5+8@  krz f2 f0+f2@  fi f2 f0+f3@ xtlo  malkrz xx iska  ata f1 1  inj f0+f3@ f0+f1@ f0+f3@  ata f3 1  l' iska  krz xx panqa  krz f2 f5+8@  l' fistir  inj f0+f1@ f0+f2@ f0+f1@    nta f5 4  krz f5@ f1  nta f5 4  krz f5@ f5+20@  nta f5 4  krz f5@ f5+20@  nta f5 4  krz f1 f5+12@  nta f1 1  krz f5@ f1  nta f5 4  inj f5@ xx ycax  ata f5 8  krz f1 f5+8@  ata f1 1  krz f5@ f1  nta f5 4  krz f5@ f5+20@  nta f5 4  inj f5@ xx ycax  ata f5 4  krz xx f5@  l' lus"
