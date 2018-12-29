module Lexer where

import Token
import Lexer.Reader

--词法二元组
type TokenTuple = (JsonToken,String)

--二元组中获取token
getToken :: TokenTuple -> JsonToken
getToken a = fst a

--二元组中获取词
getWord :: TokenTuple -> String
getWord a = snd a

--读取字符串到词法二元组列表
read2TokenTuple :: String -> [TokenTuple]
read2TokenTuple [] = []
read2TokenTuple (' ':tl) = read2TokenTuple tl --空格直接跳过
read2TokenTuple ('\n':tl) = read2TokenTuple tl --换行直接跳过
read2TokenTuple ('\r':tl) = read2TokenTuple tl --回车直接跳过
read2TokenTuple ('\t':tl) = read2TokenTuple tl --tab直接跳过
read2TokenTuple ('{':tl) = (OBJ_BEGIN,"{") : read2TokenTuple tl
read2TokenTuple ('}':tl) = (OBJ_END,"}") : read2TokenTuple tl
read2TokenTuple ('[':tl) = (ARR_BEGIN,"[") :read2TokenTuple tl
read2TokenTuple (']':tl) = (ARR_END,"]") : read2TokenTuple tl
read2TokenTuple (':':tl) = (TCOLON,":") : read2TokenTuple tl
read2TokenTuple (',':tl) = (TCOMMA,",") : read2TokenTuple tl
read2TokenTuple all@(hd:tl)
  | hd `elem` ['+','-'] ++ ['0','1'..'9'] =
    let (numbers,afternum) = readNumber all in (TNUMBER,numbers) : read2TokenTuple afternum
  | hd == '"' =
    let (strings,afterstr) = readString all in (TSTRING,strings) : read2TokenTuple afterstr
  | hd == 't' =
    let (ttrue,aftertrue) = readTrue all in (TTRUE,ttrue) : read2TokenTuple aftertrue
  | hd == 'f' =
    let (tfalse,afterfalse) = readFalse all in (TFALSE,tfalse) : read2TokenTuple afterfalse
  | hd == 'n' =
    let (tnull,afternull) = readNull all in (TNULL,tnull) : read2TokenTuple afternull
  | otherwise = error $ "can not recognize the key word like " ++ hd:"%"
