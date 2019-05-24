
{-
看了parsec之后的实现，利用parsec非常简单
-}

module NewParser where

import Control.Applicative hiding ( (<|>), many)
import Text.ParserCombinators.Parsec
import Numeric

data JValue = JString String
             |JNumber Double
             |JBool Bool
             |JNull
             |JObject (JObj JValue)
             |JArray (JAry JValue)
             deriving (Eq, Ord, Show)
newtype JAry a = JAry {
  fromJAry :: [a]
} deriving (Eq, Ord, Show)

newtype JObj a = JObj {
  fromJObj :: [(String,a)]
} deriving (Eq, Ord, Show)


parseJson :: String -> Either ParseError JValue
parseJson input = parse p_text "(unknown)" input

p_text :: CharParser () JValue
p_text = spaces *> text
  <?> "JSON text"  --解析不成功时提示的文字
  where
    text = JObject <$> p_object --解析对象
      <|> JArray <$> p_array    --解析数组

-- 解析对象和数组是相似的，都是右边一个符号'{'或'[' 中间一大段字符串，右边一个符号'}'或']'
p_series ::
  Char                 --左符号
  -> CharParser () a   --中间的parser
  -> Char              --右符号
  -> CharParser () [a] --组合成的parser
p_series left parser right =
  between (char left <* spaces) (char right) $
          (parser <* spaces) `sepBy` (char ',' <* spaces)
{-
notes:
Applicative中：<$>和fmap是同等意思
*> 接受两个Applicative，先应用第一个Applicative，但是忽略其结果，再应用第二个Applicative并返回结果
<* 则与*>相反
<*> 接受两个applicative，第一个applicative包裹了一个函数，将第二个applicative里的值应用到这个函数上
所以上面 char left <* spaces的意思是，先读取解析left，然后不断读取spaces去消耗字符串
-}

p_array :: CharParser () (JAry JValue)
p_array = JAry <$> p_series '[' p_value ']'

p_object :: CharParser () (JObj JValue)
p_object = JObj <$> p_series '{' p_field '}'
  where p_field = (,) <$> (p_string <* spaces <* char ':' <* spaces) <*> p_value

p_value :: CharParser () JValue
p_value = value <* spaces
  where
    value = JString <$> p_string
            <|> JNumber <$> p_number
            <|> JObject <$> p_object
            <|> JArray <$> p_array
            <|> JBool <$> p_bool
            <|> JNull <$ string "null"
            <?> "JSON value"
p_bool :: CharParser () Bool
p_bool = True <$ string "true"
         <|> False <$ string "false"

p_value_choice = value <* spaces
  where value = choice [ JString <$> p_string
                       , JNumber <$> p_number
                       , JObject <$> p_object
                       , JArray  <$> p_array
                       , JBool   <$> p_bool
                       , JNull   <$ string "null"
                       ]
                <?> "JSON value"

p_number :: CharParser () Double
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (p_escape <|> p_unicode)
              <|> satisfy (`notElem` "\"\\")

p_escape :: CharParser () Char
p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode :: Char -> Char -> CharParser () Char
          decode c r = r <$ char c

p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x
