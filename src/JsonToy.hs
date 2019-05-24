module JsonToy(
  fromJson,
  toJson,
  newFromJson
) where

import Parser
import System.IO
import JsonObject
import Lexer
import NewParser
import Text.ParserCombinators.Parsec hiding (parse)

fromJson :: String -> JsonElement
fromJson str = parse $ read2TokenTuple str

toJson :: JsonElement -> String
toJson element = toJsonStr element

--20190524更新的，使用parsec解析的json
newFromJson :: String -> Either ParseError JValue
newFromJson input = NewParser.parseJson input
