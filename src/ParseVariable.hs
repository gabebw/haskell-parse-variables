module ParseVariable where

import Data.Char (isUpper)

type Variable = (String, String)

parseVariable :: String -> Maybe Variable
parseVariable (' ':xs) = Nothing
parseVariable s = Just (key s, value s)

removeJunk :: String -> String
removeJunk = withoutQuotes . withoutEndingNewline . withoutBeginningExport

key :: String -> String
key = (takeWhile notEqualsSign) . removeJunk

value :: String -> String
value = reverse . takeWhile notEqualsSign . reverse . removeJunk

notEqualsSign :: Char -> Bool
notEqualsSign = (/= '=')

withoutEndingNewline :: String -> String
withoutEndingNewline = takeWhile (/= '\n')

withoutQuotes :: String -> String
withoutQuotes = filter (/='"') . filter (/= '\'')

withoutBeginningExport :: String -> String
withoutBeginningExport s
  | startsWithExport s = drop 7 s
  | otherwise = s

startsWithExport :: String -> Bool
startsWithExport s = "export" == take 6 s
