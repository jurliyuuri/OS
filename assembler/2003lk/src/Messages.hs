{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Messages
(Message(..)
,LangInfo(..)
,putStrLn'
,toString
) where
import Types

data LangInfo = English

class Message a where
 show' :: LangInfo -> a -> String

instance Message LinkError where
 show' English (LinkError str) = "LinkError: " ++ str

instance Message ParseError where
 show' English (ParseError str) = "ParseError: " ++ str

instance Message RuntimeError where
 show' English (RuntimeError str str2) = "RuntimeError: " ++ str ++ str2

instance Message NormalMessage where
 show' English (NormalMessage str) = str

putStrLn' :: LangInfo -> NormalMessage -> IO ()
putStrLn' English (NormalMessage a) = putStrLn a

toString :: LangInfo -> NormalMessage -> String
toString English (NormalMessage a) = a
