{-# LANGUAGE FlexibleInstances #-}
module Prog.Args.OptVal where

import Prog.Args.Types
import Prog.Args.Impl

import Data.Int
import Data.Word
import Data.Typeable

class OptVal v where
  ovTypeName :: v -> String
  ovFormat   :: v -> String
  ovParse    :: OptSpec o -> String -> (Either String v)

instance OptVal Bool where
  ovTypeName _ = "Bool"
  ovFormat = show
  ovParse = parseRead

instance OptVal Char where
  ovTypeName _ = "Char"
  ovFormat = show
  ovParse = parseRead

instance OptVal [Char] where
  ovTypeName _ = "String"
  ovFormat     = id
  ovParse _ s  = Right s

instance OptVal Int where
  ovTypeName _ = "Int"
  ovFormat     = show
  ovParse      = parseNum

instance OptVal Int8 where
  ovTypeName _ = "Int8"
  ovFormat     = show
  ovParse      = parseNum

instance OptVal Int16 where
  ovTypeName _ = "Int16"
  ovFormat     = show
  ovParse      = parseNum

instance OptVal Int32 where
  ovTypeName _ = "Int32"
  ovFormat     = show
  ovParse      = parseNum

instance OptVal Int64 where
  ovTypeName _ = "Int64"
  ovFormat     = show
  ovParse      = parseNum

instance OptVal Word8 where
  ovTypeName _ = "Word8"
  ovFormat     = show
  ovParse      = parseNum

instance OptVal Word16 where
  ovTypeName _ = "Word16"
  ovFormat     = show
  ovParse      = parseNum

instance OptVal Word32 where
  ovTypeName _ = "Word32"
  ovFormat     = show
  ovParse      = parseNum

instance OptVal Word64 where
  ovTypeName _ = "Word64"
  ovFormat     = show
  ovParse      = parseNum

-- instance (OptVal v1,OptVal v2) => OptVal (v1,v2) where
--  ovTypeName (v1,v2) = "(" ++ ovTypeName v1 ++ "," ++ ovTypeName v2 ++ ")"
--  ovFormat   (v1,v2) = "(" ++ ovFormat v1 ++ "," ++ ovFormat v2 ++ ")"
--  ovParse    os s = parsePair
-- need parsec
-- parsePair os s
--  | take 1 l_par == "("
--  where l_par = dropWhile isSpace s

parseNum :: (Read v,Num v,OptVal v) => OptSpec o -> String -> Either String v
parseNum o s = do
  case reads s of
    [(x,"")]          -> Right x
    [(x,c:[])]
      | has_scaling_suffixes ->
        case c of
          _ | c `elem` "kK" -> Right $ 1024 * x
            | c `elem` "mM" -> Right $ 1024 * 1024 * x
            | c `elem` "gG" -> Right $ 1024 * 1024 * 1024 * x
      where has_scaling_suffixes = not (o `osHasAttr` OptAttrNoScalingSuffixes)
    ((x,""):(y,""):_) -> Left $ "ambiguous value to " ++ optNames o ++ "; could be " ++ ovFormat x ++ " or " ++ ovFormat y
    _                 -> Left $ "malformed argument to " ++ optNames o

optNames :: OptSpec o -> String
optNames os
  | null s || null l = osFriendlyName os
  | otherwise = "option --" ++ l ++ " (or -" ++ s ++ ")"
  where s = osShort os
        l = osLong  os

-- In future, could format with "1k"
-- fmtNum :: (Show v,Num v) => Opt v -> v -> String
-- fmtNum o v =

parseRead :: (Read v,OptVal v) => OptSpec o -> String -> Either String v
parseRead o s =
  case reads s of
    [(x,"")]          -> Right x
    ((x,""):(y,""):_) -> Left $ "ambiguous value to " ++ optNames o ++ "; could be " ++ ovFormat x ++ " or " ++ ovFormat y
    _                 -> Left $ "malformed argument " ++ show s ++ " to " ++ optNames o

typeableName :: Typeable t => t -> String
typeableName = show . typeOf



-- Represents the format of option to parse.
-- data Format v = Format {
--       fmtName :: String -- name to show: e.g. FILE
--     , fmtValue :: v -> String -- format a value to text
--     , fmtParse :: String -> Either String (v,String) -- parses a value, returns error or, (value,leftover)
--     }


--
-- instance OptVal String where
--  ovName _ = "String"
--   ovFormat = id
--   ovParse = Right
--
-- instance OptVal Int where
--   ovName _ = show (typeOf (0::Int))
--   ovFormat = show
--   ovParse = useReadsTypeable
--
-- useReadsTypeable :: (Read a, Typeable a) => String -> Either String a
-- useReadsTypeable s =
--   case reads s of
--     [(s,"")] -> Right s
--     x -> Left $ "malformed " ++ show (typeOf (head x))
--
-- data EnumSet v = EnumSet { esName :: String, esFormat :: v -> String, esValues :: [(String,v)] }
-- instance OptVal (EnumSet v) where
--  ovName = esName
--  ovFormat = esFormat
--  ovParse es s = case lookup s ... of

