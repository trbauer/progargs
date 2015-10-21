{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Prog.Args.Types where

import Data.Typeable

-- TODO: specify triggers
-- Basically, we parse everything including triggers.
-- If any trigger is specified we call, then after parsing all arguments
-- we call (osDerived os) on each of the triggers and ***exit success***.
-- An example of a trigger is the --help option.
--
-- TODO: make the help option take a list of strings --help=foo,bar,baz --help foo,bar,baz
--

-- A set of option specifications.
-- The 'o' type variable corresponds to the data type containing the options
data Spec o = Spec {
    specExeName :: String
  , specTitle :: String -- title and description for this program (can be "")
  , specMaxCols :: Int -- number of columns in help screen (where possible)
  , specBadArg :: forall a . Maybe (OptSpec o) -> String -> IO a -- emits an error on an arugment or option; if the error has no option associated with it (e.g. unmatched/invalid option), Nothing is passed
  , specOpts :: [OptSpec o]
  , specArgs :: [OptSpec o]
  }

-- An option specification.  An arugment is also treated as an
-- option specification, but with empty short and long names.
-- The 'o' type variable corresponds to the data type containing the options
data OptSpec o = OptSpec {
    osShort    :: String -- the short option name (or "" if no short name),
                         -- if both this and long are "", then it's an arg. (not an opt.)
  , osLong     :: String -- the long option name (or "" if no long name)
  , osTypeName :: String -- -foo=FILE -> "FILE".  For ARGs, this gets listed as the name
  , osDesc     :: String -- the (short) description (be concise)
  , osExtDesc  :: String -- an extended description for the --help option
  , osAttrs    :: [OptAttr]
  , osDerived  :: (Maybe (o -> IO o)) -- if an option does not get set explicitly,
                                      -- this action is run at the end; if this is Nothing
                                      -- and OptAttrAllowUnset is not an attribute, then
                                      -- we throw a fit
  , osSetFlag  :: Maybe (o -> IO o) -- for flags
  , osSetUnary :: Maybe (String -> o -> IO o) -- for options taking an argument: --foo=v or --foo v
  }
-- Is an option instead of an argument
osIsOpt :: OptSpec o -> Bool
osIsOpt = not . osIsArg
-- Is an argument instead of option
osIsArg :: OptSpec o -> Bool
osIsArg os = null (osShort os) && null (osLong os)
-- tests if an argument or option has an attribute
osHasAttr :: OptSpec o -> OptAttr -> Bool
osHasAttr os = (`elem`osAttrs os)

-- These attributes are affixed to various options and arguments.
data OptAttr =
    OptAttrAllowUnset        -- an option can be left unset and without
                             -- an 'osDerived' trigger and we won't complain
  | OptAttrAllowMultiple     -- an option can be included many times, this
                             -- is useful for collecting lists of options
                             -- as well as allowing ``override'' semantics
  | OptAttrAllowFusedSyntax  -- enables parsing of the option such as -j4
                             -- as fused key value pairs (i.e. same as -j=4
                             -- only the short option name is considered in
                             -- this syntax.  In cases of ambiguity,
                             -- an exact match is favored (e.g. "-j4" would
                             -- match option "-j4" (as a flag) over "-j=4"
                             -- in cases of ambiguity a spec error is raised
  | OptAttrNoScalingSuffixes -- by default options created with 'opt' or 'arg'
                             -- permit scaling suffixes such as 'k' (or "K"),
                             -- which would scale by 1024; 'm' (or "M"), which
                             -- scales by 1024*1024; or 'g'/'G' which is *1024
                             -- more; if this options is set, we do not
                             -- recognize said suffixes
-- | OptAttrHidden           -- hidden options are not shown in the help
--                           -- message
-- For now I can implement eager triggers as a combinator
-- like 'opt'.
--  | OptAttrTrigger           -- trigger options (and arguments) are processed
                             -- at the end of parsing all other arguments
                             -- they are invoked at that time with their
                             -- arguments; triggers typically terminate
                             -- processing with 'exitSuccess' or 'exitFailure',
                             -- but this is is not strictly necessary.
  deriving (Show,Eq)

-- Represents the format of option to parse.
-- data Format v = Format {
--       fmtName :: String -- name to show: e.g. FILE
--     , fmtValue :: v -> String -- format a value to text
--     , fmtParse :: String -> Either String (v,String) -- parses a value, returns error or, (value,leftover)
--     }

-- class OptVal v where
--  ovName :: v -> String
--  ovFormat :: v -> String
--  ovParse :: String -> (Either String v)
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



