-- Idea: Parse string into ("Foo ", "=", "Bar") and then if there are spaces in
-- first element or third element, that's no good
module ParseVariable where

type Variable = (String, String)
type Key = String
type Value = String

parseVariable :: String -> Maybe Variable
parseVariable (' ':_) = Nothing
parseVariable s = Just (key, value)
  where
    key = keyAndValue !! 0
    value = keyAndValue !! 1
    keyAndValue = split s


split :: String -> [String]
split s = [left, right]
  where
    left = takeWhile (/= '=') s
    right = withoutNewline $ reverse $ takeWhile (/= '=') $ reverse s
    withoutNewline = takeWhile (/='\n')
