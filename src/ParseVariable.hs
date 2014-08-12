module ParseVariable where

type Variable = (String, String)

parseVariable :: String -> Maybe Variable
parseVariable s = Just (key s, value s)

key :: String -> String
key = takeWhile (/= '=')

value :: String -> String
value = withoutEndingNewline . reverse . key . reverse

withoutEndingNewline :: String -> String
withoutEndingNewline = takeWhile (/= '\n')
