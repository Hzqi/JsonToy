module Parser where

import Lexer
import Token
import JsonObject
import qualified Data.Map as Map

{-
json文法:

object -> { members }
array -> [ elements ]
members -> member | member , members
member -> string : element
elements -> element | element , elements
element -> value
value -> object | array | string | number | bool | null
-}

parse :: [TokenTuple] -> JsonElement
parse all@(hd:tl)
  | fst hd == OBJ_BEGIN = fst $ objectParser all
  | fst hd == ARR_BEGIN = fst $ arrayParser all
  | otherwise = error "unexpected start."
parse [] = error "can not parse with empty"


getAfterTuple :: [TokenTuple] -> JsonToken -> [TokenTuple]
getAfterTuple (hd:tl) token
  | fst hd == token = tl
  | otherwise = error $ "error : should end with " ++ show token

--object文法
objectParser :: [TokenTuple] -> (JsonElement,[TokenTuple]) --生成的对象和剩余的词法二元组
objectParser (hd:tl)
  | (fst hd) == OBJ_BEGIN =
    let (members,afterMembers@(amd:amt)) = membersParser tl Map.empty in
    let afterObject = getAfterTuple afterMembers OBJ_END in
    (JObject members, afterObject)
  | otherwise = error "error : object should begin wiht {"
objectParser [] = error "can not parse object with empty"

--members文法
membersParser :: [TokenTuple] -> Map.Map String JsonElement -> (Map.Map String JsonElement, [TokenTuple])
membersParser all@(hd:tl) jmap
  | fst hd == TCOMMA = membersParser tl jmap -- 当下一个词法是,的时候，走 member , members文法
  | fst hd == TSTRING =
    let (addedJmap,afterMember) = memberParser all jmap in
      membersParser afterMember addedJmap
  | otherwise = (jmap, all)
membersParser [] _ = error "can not parse members with empty"

--member文法
memberParser :: [TokenTuple] -> Map.Map String JsonElement -> (Map.Map String JsonElement, [TokenTuple])
memberParser all@(hd:tl) jmap
  | fst hd == TSTRING =
    let (element,afterElement) = memberParser' tl in
      (Map.insert (snd hd) element jmap, afterElement)
  | otherwise = error "error: member should begin with string"
memberParser [] _ = error "can not parse memeber with empty"

--member文法辅助
memberParser' :: [TokenTuple] -> (JsonElement, [TokenTuple])
memberParser' (hd:tl)
  | fst hd == TCOLON = elementParser tl
  | otherwise = error "error: object member should after by :"
memberParser' [] = error "can not parse member with empty"

--element文法
elementParser :: [TokenTuple] -> (JsonElement,[TokenTuple])
elementParser all@(hd:tl)
  | fst hd == TSTRING = (JString $ snd hd, tl) --字符串
  | fst hd == TNUMBER = let numberValue = read (snd hd) :: Double in (JNumber numberValue, tl)
  | fst hd == TTRUE || fst hd == TFALSE =
    let boolStr = if (snd hd) == "true" then "True" else "False" in
    let boolValue = read boolStr :: Bool in (JBool boolValue, tl)
  | fst hd == TNULL = (JNull, tl)
  | fst hd == OBJ_BEGIN = objectParser all
  | fst hd == ARR_BEGIN = arrayParser all
  | otherwise = error $ "parser error for : " ++ (show $ fst hd)
elementParser [] = error "error: can not parse element with empty"

--array文法
arrayParser :: [TokenTuple] -> (JsonElement,[TokenTuple])
arrayParser (hd:tl)
  | fst hd == ARR_BEGIN =
     let (elements, afterElements) = elementsParser tl [] in
     let afterArray = getAfterTuple afterElements ARR_END in
       (JArray $ reverse elements, afterArray)
  | otherwise = error "error: array should begin with ["
arrayParser [] = error "error: can not parse array with empty"

--elements文法
elementsParser :: [TokenTuple] -> [JsonElement] -> ([JsonElement], [TokenTuple])
elementsParser all@(hd:tl) jlist
  | fst hd == TCOMMA = elementsParser tl jlist
  | fst hd == ARR_END = (jlist,all)
  | otherwise =
    let (element, afterElement) = elementParser all in
      elementsParser afterElement (element:jlist)
elementsParser [] jlist = (jlist,[])
