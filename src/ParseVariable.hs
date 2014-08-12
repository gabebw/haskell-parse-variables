module ParseVariable where

import Data.Char (isUpper)

type Variable = (String, String)

parseVariable :: String -> Maybe Variable
parseVariable (' ':xs) = Nothing
parseVariable s = Just (key clean, value clean)
  where clean = removeJunk s

removeJunk :: String -> String
removeJunk = withoutQuotes . withoutEndingNewline . withoutBeginningExport

key :: String -> String
key = (takeWhile notEqualsSign)

value :: String -> String
value = reverse . takeWhile notEqualsSign . reverse

notEqualsSign :: Char -> Bool
notEqualsSign = (/= '=')

withoutEndingNewline :: String -> String
withoutEndingNewline = takeWhile (/= '\n')

withoutQuotes :: String -> String
withoutQuotes = filter (/='"') . filter (/= '\'')

withoutBeginningExport :: String -> String
withoutBeginningExport s
  | startsWithExport s = drop (length "export ") s
  | otherwise = s

startsWithExport :: String -> Bool
startsWithExport s = "export" == take (length "export") s
