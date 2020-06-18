module HsonSchema
( Schema
, makeSchema
) where

import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Data(toConstr)
import Data.List(groupBy)

type Schema = Json.Object

data JType = JString | JInteger | JNumber | JObject | JArray | JBoolean | JNull

makeSchema :: Json.Value -> Schema
makeSchema Json.Null     = newSchema JNull
makeSchema (Json.Bool   _) = newSchema JBoolean
makeSchema (Json.Number _) = newSchema JNumber
makeSchema (Json.String _) = newSchema JString
makeSchema (Json.Array  arr) = 
    let
        uniqueArr = uniqueByTypes arr
        uniqueLength = V.length uniqueArr
        items = fmap (Json.Object . makeSchema) $ uniqueArr
        schema = newSchema JArray `withProp` ("length", V.length arr)
    in
        schema `withProp` ("items", case uniqueLength of
            0 -> Json.Object emptyObject
            1 -> V.head items
            _ -> Json.Array items)
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

uniqueByTypes :: Json.Array -> Json.Array
uniqueByTypes = V.fromList . ungroupHeads . (groupBy typeEqual) . V.toList

typeEqual :: Json.Value -> Json.Value -> Bool
typeEqual a b = toConstr a == toConstr b

ungroupHeads :: [[a]] -> [a]
ungroupHeads [] = []
ungroupHeads ([]:xs) = ungroupHeads xs
ungroupHeads ((x:xs):ys) = x : ungroupHeads ys

jTypeToStr :: JType -> String
jTypeToStr JString  = "string"
jTypeToStr JInteger = "integer"
jTypeToStr JNumber  = "number"
jTypeToStr JObject  = "object"
jTypeToStr JArray   = "array"
jTypeToStr JBoolean = "boolean"
jTypeToStr JNull    = "null"
