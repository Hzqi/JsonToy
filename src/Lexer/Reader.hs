module Lexer.Reader where

import Data.Char

digit = ['0','1' .. '9']

--读取数字，包括正数负数，需要状态机辅助函数
readNumber :: String -> (String,String) --第一个是单词(值)，第二个是继续的列表
readNumber str = stateMachineNum 0 str []

--读取数字的状态机
stateMachineNum :: Int -> String -> String -> (String,String) -- 状态 -> 字符串 -> 临时结果 -> (结果，剩余的字符串)
-- 0
stateMachineNum 0 (hd:tl) temp
  | hd == '-' = stateMachineNum 1 tl (hd:temp)
  | hd `elem` digit = stateMachineNum 2 tl (hd:temp)
  | otherwise = error "error in state 0"
-- 1
stateMachineNum 1 (hd:tl) temp
  | hd `elem` digit = stateMachineNum 2 tl (hd:temp)
  | otherwise = error "error in state 1"
-- 2
stateMachineNum 2 all@(hd:tl) temp
  | hd `elem` digit = stateMachineNum 2 tl (hd:temp)
  | hd == '.' = stateMachineNum 3 tl (hd:temp)
  | hd == 'E' || hd == 'e' = stateMachineNum 5 tl (hd:temp)
  | otherwise = (reverse temp,all) -- 当遇到不是这个状态该有的时候，直接返回
-- 3
stateMachineNum 3 (hd:tl) temp
  | hd `elem` digit = stateMachineNum 4 tl (hd:temp)
  | otherwise = error "error in state 3"
-- 4
stateMachineNum 4 all@(hd:tl) temp
  | hd `elem` digit = stateMachineNum 4 tl (hd:temp)
  | hd == 'E' || hd == 'e' = stateMachineNum 5 tl (hd:temp)
  | otherwise = (reverse temp, all)
-- 5
stateMachineNum 5 (hd:tl) temp
  | hd `elem` digit = stateMachineNum 7 tl (hd:temp)
  | hd == '+' || hd == '-' = stateMachineNum 6 tl (hd:temp)
  | otherwise = error "error in state 5"
-- 6
stateMachineNum 6 (hd:tl) temp
  | hd `elem` digit = stateMachineNum 7 tl (hd:temp)
  | otherwise = error "error in state 6"
-- 7
stateMachineNum 7 all@(hd:tl) temp
  | hd `elem` digit = stateMachineNum 7 tl (hd:temp)
  | otherwise = (reverse temp, all)
-- default状态
stateMachineNum _ [] temp = (reverse temp, [])


-- 读取字符串
readString :: String -> (String,String) --第一个是单词，第二个是继续的列表
readString str = stateMachineStr 0 str []

--读取字符串的状态机
stateMachineStr :: Int -> String -> String -> (String,String)
-- 0 开始的状态
stateMachineStr 0 (hd:tl) temp
  | hd == '"' = stateMachineStr 1 tl temp
  | otherwise = error "error in state 0"
-- 1 双引号后的状态
stateMachineStr 1 (hd:tl) temp
  | hd == '\\' = stateMachineStr 2 tl temp
  | hd == '"' = (reverse temp, tl)
  | otherwise = stateMachineStr 1 tl (hd:temp)
-- 2 转义符(\)后的状态
stateMachineStr 2 (hd:tl) temp
  | hd == '/' = stateMachineStr 1 tl ('/':temp) --网上查到正斜杠需要转义？
  | hd == 'b' = stateMachineStr 1 tl ('\b':temp)
  | hd == 'f' = stateMachineStr 1 tl ('\f':temp)
  | hd == 't' = stateMachineStr 1 tl ('\t':temp)
  | hd == 'n' = stateMachineStr 1 tl ('\n':temp)
  | hd == 'r' = stateMachineStr 1 tl ('\r':temp)
  | hd == '"' = stateMachineStr 1 tl ('"':temp)
  | hd == 'u' = stateMachineStr 3 tl temp
  | otherwise = error "error in state 2"
-- 3 unicode转义符后的状态
stateMachineStr 3 all@(hd:tl) temp = stateMachineStr 1 nexts (uniChar : temp)
  where
    uniStr = take 4 all
    nexts = drop 4 all
    uniChar = fst . head . readLitChar $ '\\' : 'x' : uniStr
--default状态
stateMachineStr _ [] temp = (reverse temp, [])

--读取true
readTrue :: String -> (String,String)
readTrue str
  | first4 == "true" = ("true", after4)
  | otherwise = error $ "can not recognize key word : " ++ first4
  where
    first4 = take 4 str
    after4 = drop 4 str

--读取false
readFalse :: String -> (String,String)
readFalse str
  | first5 == "false" = ("false", after5)
  | otherwise = error $ "can not recognize key word : " ++ first5
  where
    first5 = take 5 str
    after5 = drop 5 str

--读取null
readNull :: String -> (String,String)
readNull str
  | first4 == "null" = ("null",after4)
  | otherwise = error $ "can not recognize key word : " ++ first4
  where
    first4 = take 4 str
    after4 = drop 4 str
