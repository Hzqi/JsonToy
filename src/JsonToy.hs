module JsonToy(
  fromJson,
  toJson
) where

import Parser
import System.IO
import JsonObject
import Lexer

fromJson :: String -> JsonElement
fromJson str = parse $ read2TokenTuple str

toJson :: JsonElement -> String
toJson element = toJsonStr element
