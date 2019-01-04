module JsonObject where

import qualified Data.Map as Map
import qualified Data.List as List

data JsonElement =
  JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JArray [JsonElement]
  | JObject (Map.Map String JsonElement)
  deriving (Show)

toJsonStr :: JsonElement -> String
toJsonStr (JString str) = '"': str ++"\""
toJsonStr (JNumber dou) = show dou
toJsonStr (JBool b) = if b == True then "true" else "false"
toJsonStr JNull = "null"
toJsonStr (JArray arr) = '[' : arrJsonStr ++ "]"
  where
    li = map (\i -> toJsonStr i) arr
    arrJsonStr = if null li then ""
      else foldl (\i j -> i ++ "," ++ j) (head li) (tail li)
toJsonStr (JObject obj) = '{' : objJsonStr ++ "}"
  where
    elems = Map.toList obj
    li = map (\i -> (toJsonStr $ JString $ fst i) ++ ":" ++ (toJsonStr $ snd i) ) elems
    objJsonStr = if null li then ""
      else foldl (\i j -> i ++ "," ++ j) (head li) (tail li)

getElementByObject :: JsonElement -> String -> Maybe JsonElement
getElementByObject (JObject jmap) name = Map.lookup name jmap
getElementByObject _ _ = Nothing

getListByArray :: JsonElement -> [JsonElement]
getListByArray (JArray arr) = arr
getListByArray _ = []

getMapByObject :: JsonElement -> Map.Map String JsonElement
getMapByObject (JObject jmap) = jmap
getMapByObject _ = Map.empty
