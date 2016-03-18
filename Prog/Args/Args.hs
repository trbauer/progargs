module Prog.Args.Args(
    module Prog.Args.Types
  -- specification construction
  , mkSpec, mkSpecsWithHelpOpt

  -- option, flag, and argument smart constructors
  , opt, optPIO, optES, optE, flag
  , arg, argPIO
  , triggerIO
  , optGroup

  -- modifiers
  , (#), (#+)
  , withAttribute, withAttributes, replaceAttributes
  , (#=), (#<-)
  , withDefault
  , withDerived
  , withNullaryHandler

    -- parsing (re-exported from Impl)
  , parseArgs

    -- occasionally useful for documentation
  , englishList

  -- lower-level stuff that might be useful for harder options
--  , customSetter, customFlagSetter, typeableReadSetter
  ) where

import Prog.Args.OptVal
import Prog.Args.Impl
import Prog.Args.Types

import Control.Exception
import Data.List
import Debug.Trace
import System.Exit
import System.IO


-- Makes a specification given a list of options and arguments
-- The function takes the executable name, a description, the max
-- number of columns to use when formatting help messages, a list of
-- options, and a list of arguments.
--
-- Construction of specifications is necessarily recursive.  Since
-- all option definitions require a pointer to the 'Spec' they are a member
-- of, one has to define the specfication similar to the recursive use
-- of 'spec'.
-- @
-- spec = mkSpec "ls" "lists files" 80 opts []
--   where opts = [opt spec "l" "list format" ...]
-- @
mkSpec :: String -> String -> Int -> [OptSpec o] -> [OptSpec o] -> Spec o
mkSpec exe desc max_cols oss ass = spec
  where spec = Spec exe desc (max 80 max_cols) (defaultBadArgHandler spec exe) oss ass
        -- I have to repass the arguments here to force unification
        -- defaultBadArgHandler :: Spec o -> String -> Maybe (OptSpec o) -> String -> IO a
        defaultBadArgHandler spec exe mos msg = do
          hPutStrLnRed stderr (exe ++ ": " ++ msg)
          case mos of
            Nothing -> return ()
            Just os
              | osIsOpt os -> hPutStr stderr $ fmtOptSpecExtDesc spec os
              | otherwise  -> hPutStr stderr $ fmtArgSpecExtDesc spec os
          exitFailure


-- Same as 'mkSpec', but adds a -h --help trigger to list help and exit.
mkSpecsWithHelpOpt :: String -> String -> Int -> [OptSpec o] -> [OptSpec o] -> Spec o
mkSpecsWithHelpOpt exe desc max_cols oss0 ass = spec
  where spec = mkSpec exe desc max_cols oss1 ass

        oss1 = helpOpt : oss0

        helpOpt =
          OptSpec "h" "help" "OPTION"
            "lists info on an option"
            ("pass the long or short option name to this option for more info;\n" ++
             "for info on an argument pass the index of the argument;\n" ++
             "by convention the first argument is 1\n" ++
             "EXAMPLES:\n" ++
             exe ++ " -h         -- lists help overview\n" ++
             exe ++ " -h=foo     -- lists help on option 'foo'\n" ++
             exe ++ " -h=-foo    -- ... same as above\n" ++
             exe ++ " -h=1       -- lists info on the first argument the program takes\n" ++
             "")
            [OptAttrAllowUnset]
            noDefault
            (Just (\_ -> printUsage spec >> exitSuccess))
            (Just (\s _ -> helpArg s))
            Nothing
            Nothing

        -- check if it's a group
        helpArg :: String -> IO o
        helpArg nm =
          case find (\os -> osShort os == nm || osLong os == nm ||
                      ("-" ++ osShort os) == nm || ("--" ++ osLong os) == nm) flattened_oss1 of
            Just os ->
              putStr (fmtOptSpecExtDesc spec os) >> exitSuccess
            Nothing ->
              case reads nm :: [(Int,String)] of
                [(x,"")]
                  | x > length ass -> specBadArg spec Nothing $ "-h argument index " ++ nm ++ " is too large"
                  | x == 0         -> printUsage spec >> exitSuccess
                  | otherwise      -> putStr (fmtArgSpecExtDesc spec (ass !! (x - 1))) >> exitSuccess
                _ -> specBadArg spec (Just helpOpt) $ "-h cannot find long or short option " ++ nm ++ ", and bad argument index"

        -- flattened :: [OptSpec o]
        flattened_oss1 = ossFlatten oss1

-- Creates an option given long and short names, description (terse and extended),
-- the type name and setter.  Uses the 'Read' instance to assign the argument
-- type except for 'String', which uses 'id' (just copies the string).
opt ::
     (OptVal a)
  => Spec o -- the spec this will belong to
  -> String -- short name
  -> String -- long name
  -> String -- type
  -> String -- short description
  -> String -- extended description
  -> (a -> o -> o) -- setter
  -> OptSpec o
opt _    snm lnm _  _    _        _
  | any ((=="-") . take 1) [snm,lnm] = dashesInOpt "opt" snm lnm
opt spec snm lnm ty desc ext_desc setter = os
  where os = OptSpec snm lnm ty desc ext_desc [] noDefault Nothing (optValSetter spec os setter) Nothing Nothing

dashesInOpt :: String -> String -> String -> a
dashesInOpt func snm lnm =
  error ("INTERNAL ARG SPEC ERROR: Util.Args.Args." ++
          func ++ "(" ++ show snm ++ " / " ++ show lnm ++
          "): options name starts with '-'")


-- Leaves the parsing to the user.
-- User can call 'specBadArg' to indicate failure.
optPIO ::
     Spec o -- the spec this will belong to
  -> String -- short name
  -> String -- long name
  -> String -- type
  -> String -- short description
  -> String -- extended description
  -> (Spec o -> String -> o -> IO o) -- setter
  -> OptSpec o
optPIO _    snm lnm _  _    _        _
  | any ((=="-") . take 1) [snm,lnm] = dashesInOpt "optPIO" snm lnm
optPIO spec snm lnm ty desc ext_desc setter = os
  where os = OptSpec snm lnm ty desc ext_desc [] noDefault Nothing (Just safeSetter) Nothing Nothing
        safeSetter s o = do
          let handler e = specBadArg spec (Just os) ("INTERNAL ARG SPEC ERROR: Util.Args.Args.optPIO: this options parser threw: " ++ show (e :: SomeException))
          setter spec s o `catch` handler


-- Typically don't need this.
-- If you really need it, use 'optPIO' and 'ovParse' (of OptVal instance)
--
-- An option with a parser given
-- optIO ::
--     (OptVal a) =>
--      Spec o -- the spec this will belong to
--   -> String -- short name
--   -> String -- long name
--   -> String -- type
--   -> String -- short description
--   -> String -- extended description
--   -> (Spec o -> String -> o -> IO o) -- setter
--   -> OptSpec o
-- optIO _    snm lnm _  _    _        _
--   | any ((=="-") . take 1) [snm,lnm] = dashesInOpt "optP" snm lnm
-- optIO spec snm lnm ty desc ext_desc setter = os
--  where os = OptSpec snm lnm ty desc ext_desc [] noDefault Nothing (Just safeSetter)
--        safeSetter s o = do
--          let handler e = specBadArg spec (Just os) ("INTERNAL ARG SPEC ERROR: Util.Args.Args.optIO: this options parser threw: " ++ show (e :: SomeException))
--          setter spec s o `catch` handler


-- Constructs an enumeration argument.
-- Uses the 'show a' for the symbol to match.
-- See the other 'opte' for more info.
optES ::
     Show a
  => Spec o -- the spec this will belong to
  -> String -- short name
  -> String -- long name
  -> String -- type
  -> String -- short description
  -> String -- extended description
  -> (a -> o -> o) -- setter
  -> [(a,String)] -- (enum value, enum desc.) matches against (show a)
  -> OptSpec o
optES _    snm lnm _  _    _        _      _
  | any ((=="-") . take 1) [snm,lnm] = dashesInOpt "optES" snm lnm
optES spec snm lnm ty desc ext_desc setter enums =
  optE spec snm lnm ty desc ext_desc setter (map (\(a,d) -> (a,show a,d)) enums)


-- Same as 'optE', but takes an explicit enumeration symbol for each enum value.
-- The long description will be appended with each enum's description given,
-- unless all those strings are empty.
-- E.g.
-- @
-- optE ... [(RED,"RED","colors things red")
--           ,(RED,"red","") -- alias for RED, not listed though
--           ,(GRN,"GRN","colors things green")]
-- @
-- Extends the long description with info about "RED" and "GRN", but not "red".
--
-- The following will not extend the long description.
-- @
-- optE ... [(RED,"RED","") -- all descs empty
--           ,(GRN,"GRN","")]
-- @
--
optE ::
     Spec o -- the spec this will belong to
  -> String -- short name
  -> String -- long name
  -> String -- type
  -> String -- short description
  -> String -- extended description
  -> (a -> o -> o) -- setter
  -> [(a,String,String)] -- (enum symbol, enum value, enum desc.)
  -> OptSpec o
optE _    snm lnm _  _    _        _      _
  | any ((=="-") . take 1) [snm,lnm] = dashesInOpt "opte'" snm lnm
optE spec snm lnm ty desc ext_desc setter enums = os
  where os = OptSpec snm lnm ty desc ext_desc2 [] noDefault Nothing (Just parser) Nothing Nothing
        parser s o =
          case find ((== s) . snd3) enums of
            Just (a,_,_) -> return $ setter a o
            Nothing -> specBadArg spec (Just os) $
                         osFriendlyName os ++ " passed invalid argument: " ++ s ++ "\n" ++
                         fmtArgSpecExtDesc spec os
        thd3 (_,_,x) = x
        snd3 (_,x,_) = x
        ext_listings = filter (not . null . thd3) enums -- those with a description
        ext_desc2
          | null ext_listings = ext_desc
          | otherwise = if null ext_desc then enum_descs else ext_desc ++ "\n" ++ enum_descs
          where enum_descs = "    valid values are:\n" ++
                              concatMap fmtEnumDesc ext_listings
                fmtEnumDesc (_,s,d) = "      " ++ padR dlen s ++ "  - " ++ d ++ "\n"
                dlen = maximum (4 : map (length . snd3) enums)

-- Creates a flag option.  Flags have the 'OptAttrAllowUnset' attribute
-- set by default since they're inherently optional.
flag :: Spec o -> String -> String -> String -> String -> (o -> o) -> OptSpec o
flag _    snm lnm _    _        _
  | any ((=="-") . take 1) [snm,lnm] = dashesInOpt "flag" snm lnm
flag spec snm lnm desc ext_desc set_flag =
    (dftOptSpec snm lnm "" desc ext_desc) {osSetFlag = customFlagSetter set_flag} `withAttribute` OptAttrAllowUnset
  where customFlagSetter :: (o -> o) -> Maybe (o -> IO o)
        customFlagSetter f = Just $ \o -> return (f o)

dftOptSpec ::
     String -- short name
  -> String -- long name
  -> String -- type
  -> String -- short description
  -> String -- extended description
  -> OptSpec o
dftOptSpec snm lnm desc typ ext_desc =
  OptSpec snm lnm desc typ ext_desc [] Nothing Nothing Nothing Nothing Nothing

-- Creates an argument.
--
-- Arguments are defined by position.  An argument can add the attribute 'OptAttrAllowMultiple'
-- to accept the tail of the command line arguments.  Note, that it would be an error to
-- define another argument specification after one with 'OptAttrAllowMultiple', since
-- any following that would never be reached.
arg ::
     (OptVal a)
  => Spec o -- the spec this will belong to
  -> String -- type
  -> String -- short description
  -> String -- extended description
  -> (a -> o -> o) -- setter
  -> OptSpec o
arg spec ty desc ext_desc setter = os
  where os = (dftOptSpec "" "" ty_str desc ext_desc) {osSetUnary = optValSetter spec os setter}
        ty_str = if null ty then "ARG" else ty


-- Leaves the parsing to the user.
-- User can call 'specBadArg' to indicate failure.
argPIO ::
     Spec o -- the spec this will belong to
  -> String -- type
  -> String -- short description
  -> String -- extended description
  -> (Spec o -> String -> o -> IO o) -- setter
  -> OptSpec o
argPIO spec ty desc ext_desc setter = os
  where os = (dftOptSpec "" "" ty_str desc ext_desc) {osSetUnary = Just safeSetter}
        ty_str = if null ty then "ARG" else ty
        safeSetter s o = do
          let handler e = specBadArg spec (Just os) ("INTERNAL ARG SPEC ERROR: Util.Args.Args.argPIO: this options parser threw: " ++ show (e :: SomeException))
          setter spec s o `catch` handler


-- A trigger is just like a flag (with AllowUnset), but takes a simple IO action
-- to run instead of a mutator for the options.  It can observe the options,
-- but does not modify them.
-- They typically exit the program with 'exitSuccess', but don't
-- necessarily have to do so.
--
-- Triggers are typically used for help and version options.
triggerIO ::
     Spec o -- the spec this belongs to
  -> String -- short name
  -> String -- long name
  -> String -- short description
  -> String -- extended description
  -> (o -> IO o) -- the action to run
  -> OptSpec o
triggerIO _    snm lnm _    _        _
  | any ((=="-") . take 1) [snm,lnm] = dashesInOpt "triggerIO" snm lnm
triggerIO spec snm lnm desc ext_desc handler =
    OptSpec snm lnm "" desc ext_desc [OptAttrAllowUnset] noDefault (Just handler) Nothing Nothing Nothing


noDefault :: Maybe (o -> IO o)
noDefault = Nothing


-- An option group
--
--   => Cannot contains nested groups
--   => Cannot contain arg's
optGroup ::
     Spec o -- spec
  -> String -- symbol
  -> String -- short desc, e.g. "experimental options"
  -> String -- long desc
  -> [OptSpec o] -- members
  -> OptSpec o
optGroup spec sym desc ext_desc ms
  | null sym =
    error ("INTERNAL ARG SPEC ERROR: Util.Args.Args." ++
            "group(" ++ show sym ++ "): " ++
            "requires a short or long name")
  | head sym == '-' = dashesInOpt "group" sym ""
  | any osIsArg ms =
    case filter osIsArg ms of
      (os:_) ->
        error ("INTERNAL ARG SPEC ERROR: Util.Args.Args." ++
              "group(" ++ show sym ++"): " ++
              "nested groups may not contain args:\n" ++ show os)
  | any osIsGroup ms =
    case filter isGroup_HACK ms of
      (os:_) ->
        error ("INTERNAL ARG SPEC ERROR: Util.Args.Args." ++
              "group(" ++ show sym ++"): " ++
              "nested groups not supported; contains group:\n" ++ show os)
  | otherwise = grp # OptAttrAllowUnset
  where grp = (dftOptSpec sym "" "" desc ext_desc) {
                -- ensure no group contains a group
                osMaybeMembers = Just $ map f ms
              }
        -- f :: OptSpec o -> OptSpec o
        f os =
          os {
            osOwner = Just grp
          , osShort = checkNull sym (osShort os)
          , osLong = checkNull sym (osLong os)
          }

        checkNull pfx sfx
          | null sfx = ""
          | otherwise = pfx ++ sfx

        isGroup_HACK os = not (null (osMembers os))

-- adds a default value to an option
infixl 1 #=
-- sets the default value used if the option is not parsed
(#=) :: OptSpec o -> (o -> o) -> OptSpec o
(#=) = withDefault
-- nominative form of '$='
withDefault :: OptSpec o -> (o -> o) -> OptSpec o
withDefault os f = os {osDerived = Just (\o -> return (f o))}

infixl 1 #<-
(#<-) :: OptSpec o -> (o -> IO o) -> OptSpec o
(#<-) = withDefaultIO
-- sets the default value used if the option is not parsed (in IO context)
withDefaultIO :: OptSpec o -> (o -> IO o) -> OptSpec o
withDefaultIO os f = os {osDerived = Just f}


withDerived :: OptSpec o -> (o -> IO o) -> OptSpec o
withDerived os s =
  case osDerived os of
    Just d -> os { osDerived = Just (\o -> d o >>= s) }
    Nothing -> os { osDerived = Just s }


-- sets a nullary setter (e.g. this makes "-h=foo" and "-h" both valid)
withNullaryHandler :: OptSpec o -> (o -> o) -> OptSpec o
withNullaryHandler os f = os {osSetFlag = Just (\o -> return (f o))}
-- symmetric opposite to 'withNullary'
-- withUnaryHandler :: OptVal a => OptSpec o -> (a -> o -> o) -> OptSpec o
-- withUnaryHandler os f = os {osSetUnary = optValSetter spec os f}


infixl 1 #
-- appends to the attributes of an OptSpec
(#) :: OptSpec o -> OptAttr -> OptSpec o
(#) = withAttribute
-- nominative form of '#+'
withAttribute :: OptSpec o -> OptAttr -> OptSpec o
withAttribute os as = os { osAttrs = osAttrs os ++ [as] }

infixl 1 #+
-- appends to the attributes of an OptSpec
(#+) :: OptSpec o -> [OptAttr] -> OptSpec o
(#+) = withAttributes
-- nominative form of '#+'
withAttributes :: OptSpec o -> [OptAttr] -> OptSpec o
withAttributes os as = os { osAttrs = osAttrs os ++ as }


-- replaces an optspec attributes
replaceAttributes :: OptSpec o -> [OptAttr] -> OptSpec o
replaceAttributes os as = os { osAttrs = as }



optValSetter :: OptVal a => Spec o -> OptSpec o -> (a -> o -> o) -> Maybe (String -> o -> IO o)
optValSetter spec os f = optValSetterIO spec os fIO
  where fIO a o = return $ f a o


optValSetterIO :: OptVal a => Spec o -> OptSpec o -> (a -> o -> IO o) -> Maybe (String -> o -> IO o)
optValSetterIO spec os f = Just $ \s o ->
    case ovParse os s of
      Left "" -> badArg $ "malformed value " ++ s ++ " to " ++ osFriendlyName os
      Left e  -> badArg e
      Right a -> f a o
  where badArg = specBadArg spec (Just os)



-- move to args framework somewhere
englishList :: [String] -> String
englishList [] = ""
englishList [a] = a
englishList [a,b] = a ++ " or " ++ b
englishList abcs = intercalate ", " (init abcs) ++ ", or " ++ last abcs


splitStringsOn :: (Char -> Bool) -> String -> [String]
splitStringsOn _ ""  = []
splitStringsOn f str = go [] str
  where go rcs [] = [reverse rcs]
        go rcs (c:cs)
          | f c       = reverse rcs : go [] cs
          | otherwise = go (c:rcs) cs


