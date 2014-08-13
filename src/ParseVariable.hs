-- Idea: Parse string into ("Foo ", "=", "Bar") and then if there are spaces in
-- first element or third element, that's no good
module ParseVariable where

type Variable = (String, String)

parseVariable :: String -> Maybe Variable
parseVariable (' ':_) = Nothing
parseVariable s
  | hasMatchingQuotes valueWithQuotes = Just (key clean, value clean)
  | otherwise = Nothing
    where
      valueWithQuotes = value clean
      clean = removeJunk s

removeJunk :: String -> String
removeJunk = withoutEndingNewline . withoutBeginningExport

key :: String -> String
key = takeWhile notEqualsSign

value :: String -> String
value = reverse . takeWhile notEqualsSign . reverse

notEqualsSign :: Char -> Bool
notEqualsSign = (/= '=')

withoutEndingNewline :: String -> String
withoutEndingNewline = takeWhile (/= '\n')

withoutBeginningExport :: String -> String
withoutBeginningExport s
  | startsWithExport s = drop (length "export ") s
  | otherwise = s

hasMatchingQuotes :: String -> Bool
hasMatchingQuotes ('"':xs) = last xs == '"'
hasMatchingQuotes ('\'':xs) = last xs == '\''
hasMatchingQuotes (x:xs) = not lastIsQuote
  where
    lastIsQuote = last xs == '\'' || last xs == '"'

startsWithExport :: String -> Bool
startsWithExport s = "export" == take (length "export") s
