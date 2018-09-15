{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Messages
(Message(..)
,LangInfo(..)
) where
import Types

data LangInfo = English | Lineparine

dummy1 :: String
dummy1 = "fhasfa nix."

dummy2 :: String
dummy2 = "ers farkzirvhi."

class Message a where
 show' :: LangInfo -> a -> String

instance Message LinkError where
 show' English (LinkError (Eng str) (Lpa _)) = "LinkError: " ++ str
 show' Lineparine (LinkError (Eng _) (Lpa str)) = "nixo melsrolfeno: " ++ str


instance Message ParseError where
 show' English (ParseError str) = "ParseError: " ++ str
 show' Lineparine (ParseError _) = "nixo kakitercenust: " ++ dummy1

instance Message RuntimeError where
 show' English (RuntimeError str str2) = "RuntimeError: " ++ str ++ str2
 show' Lineparine (RuntimeError _ _) = "nixo ingglimj: " ++ dummy1

instance Message NormalMessage where
 show' English (NormalMessage str) = str
 show' Lineparine (NormalMessage _) = dummy2




