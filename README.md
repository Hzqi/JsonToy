# JsonToy

This is a toy for parsing Json in Haskell.

这是一个用Haskell写的Json解析玩具。

###Usage
Import the module ```JsonToy``` ,and then you can parse Json string to Json types with function```fromJson```. And you can generate to Json string with function ```toJson```.

```haskell
module Main where
import JsonToy
...

-- parse json
jsonElement = fromJson str

-- generate json
str = toJson jsonElement
```

###用法
导入```JsonToy```模块，然后可以使用函数```fromJson```解析json字符串成对应的Json类型。也可以使用函数```toJson```将Json类型值生成json字符串。

```haskell
module Main where
import JsonToy
...

-- 解析 json
jsonElement = fromJson str

-- 生成 json
str = toJson jsonElement
```

###Json Types
Json Types in Haskell like :

```
data JsonElement =
  JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JArray [JsonElement]
  | JObject (Map.Map String JsonElement)
  deriving (Show)
```  

###Json 类型
在Haskell对应的Json类型如下：

```
data JsonElement =
  JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JArray [JsonElement]
  | JObject (Map.Map String JsonElement)
  deriving (Show)
```
