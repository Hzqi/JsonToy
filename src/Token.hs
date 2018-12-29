module Token where

--Json的token
data JsonToken =
  OBJ_BEGIN | OBJ_END -- '{'  '}'
  | ARR_BEGIN | ARR_END -- '[' ']'
  | TSTRING | TCOMMA -- 字符串 "引号
  | TNUMBER --数字
  | TNULL -- 空
  | TTRUE | TFALSE -- true false
  | TCOLON -- :冒号
  deriving (Show)
