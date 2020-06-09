module HsonSchema
( Schema
, makeSchema
) where

import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

type Schema = Json.Object

data JType = JString | JInteger | JNumber | JObject | JArray | JBoolean | JNull

makeSchema :: Json.Value -> Schema
makeSchema Json.Null     = newSchema JNull
makeSchema (Json.Bool   _) = newSchema JBoolean
makeSchema (Json.Number _) = newSchema JNumber
makeSchema (Json.String _) = newSchema JString
makeSchema (Json.Array  arr) =
    newSchema JArray
    `withProp` ("length", V.length arr)
    `withProp` ("items", Json.Array $ fmap (Json.Object . makeSchema) arr)
makeSchema (Json.Object map) =
    newSchema JObject
    `withProp` ("size", M.size map)
    `withProp` ("properties", Json.Object $ fmap (Json.Object . makeSchema) map)

emptyObject :: Json.Object
emptyObject = M.empty

withProp :: (Json.ToJSON a) => Json.Object -> (String, a) -> Json.Object
obj `withProp` (key, val) = M.insert (T.pack key) (Json.toJSON val) obj

newSchema :: JType -> Schema
newSchema t = emptyObject `withProp` ("type", jTypeToStr t)

jTypeToStr :: JType -> String
jTypeToStr JString  = "string"
jTypeToStr JInteger = "integer"
jTypeToStr JNumber  = "number"
jTypeToStr JObject  = "object"
jTypeToStr JArray   = "array"
jTypeToStr JBoolean = "boolean"
jTypeToStr JNull    = "null"
