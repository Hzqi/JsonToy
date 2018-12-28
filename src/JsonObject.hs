module JsonObject where

--json元素
class JsonElement a where
  toJson :: a -> String

--null
data JsonNull = JsonNull

instance JsonElement JsonNull where
  toJson JsonNull = "null"

--bool
data JsonBool = JsonTrue | JsonFalse

instance JsonElement JsonBool where
  toJson JsonTrue = "true"
  toJson JsonFalse = "false"
