module RW.Chap5.PrettyPrint (
  renderJValue
) where

import RW.Chap5.SimpleJSON
import RW.Chap5.Prettify

renderJValue :: JValue -> String
renderJValue (JString s)    = string s
renderJValue (JBool True)   = text "true"
renderJValue (JBool False)  = text "false"
renderJValue JNull          = text "null"
renderJValue (JNumber n)    = double n
renderJValue (JArray arr)   = series '[' ']' renderJValue arr
renderJValue (JObject o)    = series '{' '}' 


