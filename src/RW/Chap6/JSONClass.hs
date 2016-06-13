{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RW.Chap6.JSONClass where

import RW.Chap5.SimpleJSON

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool 
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "Could not parse a JBool"

instance JSON String where
  toJValue = JString
  fromJValue (JString s) = Right s
  fromJValue _           = Left "Could not parse a JString" 
