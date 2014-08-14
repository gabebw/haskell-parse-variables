module ParseVariable where

import Data.Maybe (fromJust)

type Variable = (String, String)
type Key = String
type Value = String

parseVariable :: String -> Maybe Variable
parseVariable (' ':_) = Nothing
parseVariable s = processVariable (key withoutExport, value withoutExport)
  where
    withoutExport = removeFromBeginningOf "export " s

processVariable :: (Maybe Key, Maybe Value) -> Maybe Variable
processVariable (Nothing, _) = Nothing
processVariable (_, Nothing) = Nothing
processVariable (Just k, Just v) = Just (k, v)

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
    left = removeFromBeginningOf "export " $ fst $ splitOnEquals s

value :: String -> Maybe Value
value (' ':_) = Nothing
value s
  | hasSpace s = Nothing
  | otherwise = Just right
  where
    right = snd $ splitOnEquals s

hasSpace :: String -> Bool
hasSpace = (' ' `elem`)

matchingQuotes :: Value -> Bool
matchingQuotes ('\'':xs) = last xs == '\'' && not (containsSingleQuote (init xs))
matchingQuotes ('"':xs) = last xs == '"' && not (containsDoubleQuote (init xs))
matchingQuotes v = not ((containsDoubleQuote v) || (containsSingleQuote v))

containsSingleQuote :: Value -> Bool
containsSingleQuote s = '\'' `elem` s

containsDoubleQuote :: Value -> Bool
containsDoubleQuote s = '"' `elem` s

removeFromBeginningOf :: String -> Value -> Value
removeFromBeginningOf s v
  | hasPrefix = drop (length s) v
  | otherwise = v
  where
    hasPrefix = take (length s) v == s
