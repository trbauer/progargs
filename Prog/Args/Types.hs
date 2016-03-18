{-# LANGUAGE Rank2Types #-}
module Prog.Args.Types where

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
data OptSpec o =
    OptSpec {
      osShort    :: String -- the short option name (or "" if no short name),
                           -- if both this and long are "", then it's an arg. (not an opt.)
    , osLong     :: String -- the long option name (or "" if no long name)
    , osTypeName :: String -- -foo=FILE -> "FILE".  For ARGs, this gets listed as the name
    , osDesc     :: String -- the (short) description (be concise)
    , osExtDesc  :: String -- an extended description for the --help option
    , osAttrs    :: [OptAttr]
    , osDerived  :: Maybe (o -> IO o) -- if an option does not get set explicitly,
                                        -- this action is run at the end; if this is Nothing
                                        -- and OptAttrAllowUnset is not an attribute, then
                                        -- we throw a fit
    , osSetFlag  :: Maybe (o -> IO o) -- for flags
    , osSetUnary :: Maybe (String -> o -> IO o) -- for options taking an argument: --foo=v or --foo v

    -- only valid for Group (will be lifted outwards)
    , osMaybeMembers :: Maybe [OptSpec o]
    -- only options and arguments will have an owner, groups will not
    , osOwner :: Maybe (OptSpec o)
    }
osMembers :: OptSpec o -> [OptSpec o]
osMembers os = case osMaybeMembers os of {Nothing -> []; Just ms -> ms}

-- Is an optspec representing a group
osIsGroup :: OptSpec o -> Bool
osIsGroup os = case osMaybeMembers os of { Nothing -> False; _ -> True }
-- Is an option instead of an argument
osIsOpt :: OptSpec o -> Bool
osIsOpt os = not (osIsGroup os) && not (osIsArg os)
-- Is an argument instead of option
osIsArg :: OptSpec o -> Bool
osIsArg os = not (osIsGroup os) && null (osShort os) && null (osLong os)
-- tests if an argument or option has an attribute
osHasAttr :: OptAttr -> OptSpec o -> Bool
osHasAttr a = (a`elem`) . osAttrs

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
--  | OptAttrHidden            -- hidden options are not shown in the help



-- | OptExtGroup String      -- This allows one to define an option as part of an
--                           -- option group.  For instance, we could use "-X" to
--                           -- group all "extended" options as a group.
--                           -- -X or -hX would print the help for that set.
-- TODO: HAVE TO WORRY ABOUT AMBIGUITY ...
--
                           -- message
-- | OptExtended             -- Extended options are a special option group
--                           -- that is not fully shown in the help screen;
--                           -- all are prefixed with "-X" and just "-X" prints
--                           -- help just for the extended options; this generally
--                           -- corresponds to less common options.
--   OptExperimental         -- Same as extended exception the prefix is "-XX"
--
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



instance Show (OptSpec o) where
  show (OptSpec snm lnm desc exdesc tynm attrs mderiv msetfl msetu ms osown) =
    "OptSpec {\n" ++
    "  osShort = " ++ show snm ++ ",\n" ++
    "  osLong = " ++ show lnm ++ ",\n" ++
    "  osTypeName = " ++ show tynm ++ ",\n" ++
    "  osDesc = " ++ show desc ++ ",\n" ++
    "  osExtDesc = " ++ show exdesc ++ ",\n" ++
    "  osAttrs = " ++ show attrs ++ ",\n" ++
    "  osDerived = " ++ showM mderiv ++ ",\n" ++
    "  osSetFlag = " ++ showM msetfl ++ ",\n" ++
    "  osSetUnary = " ++ showM msetu ++ "\n" ++
    "  osMMembers = ...\n" ++
    "  osOwner = ...\n" ++
    "}"
    where showM (Just _) = "Just (...function...)"
          showM _ = "Nothing"