module RW.Chap5.SimpleJSON (
  JValue(..),
  getString,
  getBool, 
  getDouble,
  getInt,
  getObject,
  getArray,
  isNull
) where


type AssocList a = [(String, a)]
data JValue = JString String
            | JNumber Double
            | JBool   Bool
            | JNull
            | JObject (AssocList JValue)
            | JArray [JValue]
            deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber a) = Just a
getDouble _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber a) = Just (truncate a)
getInt _ = Nothing

getArray (JArray a) = Just a
getArray _  = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

isNull v = v == JNull
