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
    key = fst keyAndValue
    value = snd keyAndValue
    keyAndValue = splitOnEquals s

splitOnEquals :: String -> (Key, Value)
splitOnEquals s = (left, right)
  where
    left = takeWhile (/= '=') s
    right = withoutNewline $ reverse $ takeWhile (/= '=') $ reverse s
    withoutNewline = takeWhile (/='\n')

matchingQuotes :: Value -> Bool
matchingQuotes ('\'':xs) = last xs == '\'' && not (containsSingleQuote (init xs))
matchingQuotes ('"':xs) = last xs == '"' && not (containsDoubleQuote (init xs))
matchingQuotes v = not ((containsDoubleQuote v) || (containsSingleQuote v))

containsSingleQuote s = '\'' `elem` s
containsDoubleQuote s = '"' `elem` s
