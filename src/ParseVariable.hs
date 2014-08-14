module ParseVariable where

type Variable = (String, String)
type Key = String
type Value = String

parseVariable :: String -> Maybe Variable
parseVariable (' ':_) = Nothing
parseVariable s = processVariable (key withoutExport, value withoutExport)
  where
    withoutExport = removeFromBeginningOf "export " s

processVariable :: (Maybe Key, Maybe Value) -> Maybe Variable
processVariable (Just k, Just v) = Just (k, v)
processVariable _ = Nothing

splitOnEquals :: String -> (Key, Value)
splitOnEquals s = (left, right)
  where
    left = takeWhile (/= '=') s
    right = withoutNewline $ reverse $ takeWhile (/= '=') $ reverse s
    withoutNewline = takeWhile (/='\n')

key :: String -> Maybe Key
key s
  | hasSpace left = Nothing
  | otherwise = Just left
  where
    left = fst $ splitOnEquals s

value :: String -> Maybe Value
value (' ':_) = Nothing
value s
  | hasSpace right = Nothing
  | mismatchedQuotes right = Nothing
  | otherwise = Just $ removeQuotes right
  where
    right = snd $ splitOnEquals s

hasSpace :: String -> Bool
hasSpace = (' ' `elem`)

mismatchedQuotes :: Value -> Bool
mismatchedQuotes ('\'':xs) = last xs /= '\''
mismatchedQuotes ('"':xs) = last xs /= '"'
mismatchedQuotes v = containsDoubleQuote v || containsSingleQuote v

containsSingleQuote :: Value -> Bool
containsSingleQuote s = '\'' `elem` s

containsDoubleQuote :: Value -> Bool
containsDoubleQuote s = '"' `elem` s

removeQuotes :: Value -> Value
removeQuotes = filter (/='\'') . filter (/='"')

removeFromBeginningOf :: String -> Value -> Value
removeFromBeginningOf s v
  | hasPrefix = drop (length s) v
  | otherwise = v
  where
    hasPrefix = take (length s) v == s
