module Prog.Args.Impl(
  -- argument parser algorithm
    parseArgs

  -- spec formatting and printing
  , printUsage
  , fmtSpec
  , fmtOptSpecExtDesc
  , fmtArgSpecExtDesc

  -- externally used (Util.Arg)
  , osFriendlyName
  , ossFlatten

  -- lower level IO
  , hPutStrRed, hPutStrLnRed

  -- utility functions
  , padR

  ) where

import Prog.Args.Types

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Data.Typeable
import Debug.Trace
import System.Exit
import System.IO
import Text.Printf

import qualified System.Console.ANSI as SCA -- from ansi-terminal


osFriendlyName :: OptSpec o -> String
osFriendlyName opt = let notNull = not . null in
  if notNull (osLong opt) then "option --" ++ osLong opt
    else if notNull (osShort opt) then "option -" ++ osShort opt
      else if notNull (osTypeName opt) then "argument " ++ osTypeName opt
        else "argument \"" ++ osDesc opt ++ "\""


fmtSpec :: Spec o -> String
fmtSpec spec = result
  where result =
          spec_title_str ++
          "usage: " ++ specExeName spec ++ " " ++ opts_cmd_line ++ opts_args_space ++ args_cmdline ++ "\n" ++
          where_str ++
          opts_overview ++
          args_header ++
          args_overview

        spec_title_str
          | null s = ""
          | otherwise = wrapText "" (specMaxCols spec) s ++ "\n"
          where s = specTitle spec

        (opts,args) = (specOpts spec, specArgs spec)

        -- the start of the command line
        -- usage: foo.exe [OPTIONS] ARGS
        -- where OPTIONS:
        --    ...
        opts_cmd_line = if null opts then "" else "[OPTIONS]"
        opts_args_space = if null opts && null args then "" else " "
        (where_str,args_header)
          | not (null opts) = ("where OPTIONS:\n",if null args then "" else "and ARGS:\n")
          | not (null args) = ("where ARGS:\n","")
          | otherwise = ("","")
        args_cmdline
          | null args = "" -- don't need to say ARGS on the command line
          | length short_line < 48 = short_line -- just a few; put em on the line
          | otherwise = "ARGS"
          where short_line = intercalate " " (map fmtArgName args)
                fmtArgName as
                  | osHasAttr OptAttrAllowUnset as = "[" ++ type_str ++ "]"
                  | otherwise = type_str
                  where type_str = if null s then "ARG" else s
                          where s = osTypeName as
        -- the detail section for options
        opts_overview = fmtOptsOverview opts
        -- the detail section of the args
        args_overview = concatMap fmtAs args
          where fmtAs as -- only suffix with \n if the description doesn't include it
                  | null s         = ""
                  | last s == '\n' = s
                  | otherwise      = s ++ "\n"
                  where s = fmtArgSpecC (computeArgColumnWidth  spec) as

-- this gets reused in the help
fmtOptsOverview :: [OptSpec o] -> String
fmtOptsOverview os = concatMap fmtOs os
  where fmtOs o -- only suffix with \n if the description doesn't include it
          | null s         = ""
          | last s == '\n' = s
          | otherwise      = s ++ "\n"
          where s = fmtOptSpecC (computeOptsColumnWidths os) o

computeOptColumnWidths :: Spec o -> (Int,Int)
computeOptColumnWidths spec = computeOptsColumnWidths (specOpts spec)

computeOptsColumnWidths :: [OptSpec o] -> (Int,Int)
computeOptsColumnWidths os = (maximum ss, maximum ls)
  where (ss,ls) = unzip $ map ((length *** length) . fmtOptShortAndLong) os
computeArgColumnWidth :: Spec o -> Int
computeArgColumnWidth spec = maximum (map (length . osTypeName) (specOpts spec))

fmtOptSpecExtDesc :: Spec o -> OptSpec o -> String
fmtOptSpecExtDesc spec os
  | osIsGroup os = fmtOptSpecExtDescC (computeOptsColumnWidths (osMembers os)) (specMaxCols spec) os
  | otherwise = fmtOptSpecExtDescC (computeOptColumnWidths spec) (specMaxCols spec) os
fmtArgSpecExtDesc ::  Spec o -> OptSpec o -> String
fmtArgSpecExtDesc spec = fmtArgSpecExtDescC (computeArgColumnWidth spec) (specMaxCols spec)

fmtOptSpecExtDescC :: (Int,Int) -> Int -> OptSpec o -> String
fmtOptSpecExtDescC cws = fmtOptArgSpecExtDescC (fmtOptSpecC cws)
fmtArgSpecExtDescC :: Int -> Int -> OptSpec o -> String
fmtArgSpecExtDescC acw = fmtOptArgSpecExtDescC (fmtArgSpecC acw)


-- handles both options and arguments
fmtOptArgSpecExtDescC :: (OptSpec o -> String) -> Int -> OptSpec o -> String
fmtOptArgSpecExtDescC fmt max_cols oas
  | osIsGroup oas = "OPTION GROUP: " ++ osDesc oas ++ ": -" ++ osShort oas ++ "...\n" ++ fmtOptsOverview (osMembers oas)
  | otherwise = fmt oas ++ ext_desc
  where ext_desc
          | null edesc = ""
          | otherwise = "\n" ++ wrapText "    " max_cols edesc
          where edesc = osExtDesc oas


-- Formats the extended descriptor by tokenizing it and wrapping it
-- at the desired column max (passed in)
--    "foo bar baz; qux..."
--                   ^ suppose this is the column limit
-- then this wraps on spacing token
-- e.g.
--    "foo bar baz;
-- Lines starting indented get wrapped to that indentation point,
-- and lines starting with bullet points get extra indentation to after the bullet
--  ["    ","foo",...] wraps with extra indent "    "
--  ["    ","1."," ","my bullet point"] wraps to length of chars up to "my bullet point"
-- Bullet points are currently defined below as isBulletToken.
-- Informally, they are things like "*", "a.", "k)", or even things like "(a)"
--
-- Arguments:
--   * ind is the default indent added to all lines
--   * ncols0 is the max column width (we ensure this is at least > length ind + 1)
--   * ext_desc is the string we are going to wrap
wrapText :: String -> Int -> String -> String
wrapText ind ncols0 ext_desc = unlines (map fmtLine (lines ext_desc))
  where ncols = max ncols0 (length ind + 1)

        isBulletToken :: String -> Bool
        isBulletToken tk
          | tk `elem` ["*","-","=>"] = True
          | otherwise = isBulletTokenWith [".",")",":"] tk
        isBulletTokenWith bullet_set tk
          | all isAlpha (take 1 tk) && isBulletSfx (drop 1 tk) = True -- "a. bullet point" or "a) bullet point"
          | take 1 tk == "(" = isBulletTokenWith [")"] (drop 1 tk) -- "(a) ..."
          | take 1 tk == "[" = isBulletTokenWith ["]"] (drop 1 tk) -- "[a]
          | not (null digs) && isBulletSfx digs_dot = True -- "2. bullet point"
          | otherwise = False
          where (digs,digs_dot) = span isDigit tk
                isBulletSfx = (`elem` bullet_set)
        isSpaceToken :: String -> Bool
        isSpaceToken tk = all isSpace tk

        fmtLine :: String -> String
        fmtLine ln = ind ++ fmtTks (length ind) ln_tks
          where ln_tks = tokenize ln
                extra_ind = replicate (go ln_tks) ' '
                  where go (t0:t1:t2:_) -- e.g. ["  ","2."," ",...]
                          | isSpaceToken t0 && isBulletToken t1 = length t0 + length t1 + length t2
                        go (t0:t1:_) -- e.g. ["2."," ",...]
                          | isBulletToken t0 = length t0 + length t1
                        go (t0:_)
                          -- "   foo bar baz"
                          | isSpaceToken t0 = length t0
                        go _ = 0 -- "foo bar baz"

                fmtTks :: Int -> [String] -> String
                fmtTks _ [] = ""
                fmtTks k (t:ts)
                  | k + length t <= ncols = t   ++ fmtTks (k + length t) ts -- normal case
                  | otherwise             = str ++ fmtTks (length str)   ts -- newline case
                  where str
                          | isSpaceToken t = "\n" ++ ind ++ extra_ind
                          | otherwise      = "\n" ++ ind ++ extra_ind ++ t


tokenize :: String -> [String]
tokenize = tokens isDelim
  where isDelim c = isSpace c -- || c `elem` ",;.:({<"
tokens :: (a -> Bool) -> [a] -> [[a]]
tokens _  [] = []
tokens p  (a:as) = span (p a) [a] as
  where span _ spn [] = [reverse spn]
        span z spn (a:as)
          | p a == z = span z (a:spn) as
          | otherwise = reverse spn : span (not z) [a] as

fmtOptSpecC :: (Int,Int) -> OptSpec o -> String
fmtOptSpecC (sw,lw) os = opt_symbols ++ desc_str
  where opt_symbols = "  " ++ padR sw short_opt_str ++ " " ++ padR lw long_opt_str ++ " "
        desc_str = osDesc os
        (short_opt_str,long_opt_str) = fmtOptShortAndLong os
fmtOptShortAndLong :: OptSpec o -> (String,String)
fmtOptShortAndLong os = (short_opt_str, long_opt_str)
  where short_opt_str :: String
        short_opt_str =
          case osShort os of
            "" -> ""
            s -> "-" ++ s ++ type_str
        long_opt_str :: String
        long_opt_str =
          case osLong os of
            "" -> ""
            l -> "--" ++ l ++ if is_flag then "" else if null (osShort os) then "=" ++ type_str else "=..."
        is_flag =
          case osSetUnary os of
            Nothing -> True
            _ -> False
        type_str :: String
        type_str =
            case osSetUnary os of
             Just _ -> case osTypeName os of
                        "" -> "=..."
                        t -> "=" ++ t
             Nothing
              | is_grp -> "*"
              | otherwise -> "..."
              where is_grp = not (null (osMembers os))
fmtArgSpecC :: Int -> OptSpec o -> String
fmtArgSpecC cw as = "  " ++ padR cw (osTypeName as) ++ " " ++ osDesc as


padR :: Int -> String -> String
padR w s = s ++ replicate (w - length s) ' '

-- Parses the specification against a command line.
-- The algorithm proceeds walks through the argument tokens once.
--
--  (1) if a token starts with a -o or --o, we try and match it against a long or short name o (respectively)
--      IF the token is of the form -o=v, we split off the v as the value to parse
--        when setting the option (if there isn't a setter that takes a value, it's an error)
--      ELSE if there's a flag setter, we use that
--      ELSE we consume the next token as the value v
--
--      IF the option o does not exist, it's an error
--      ELSE IF o has not been specified, we accept it.
--           ELSE IF it has the allow-multi attribute, we accept it.
--           ELSE we raise an error about respecification of o
--
--  (2) otherwise we treat the argument as the next "argument" and match it against that.
--      if there are no more arguments to match, we raise an error
--      otherwise we match the argument and step to the next arg to match against
--      except when we are on the last argument and it allows multi, then we
--      don't step, but stay there waiting for repeated matches
--
-- After processing all tokens we
parseArgs :: Spec o -> o -> [String] -> IO o
parseArgs spec opts as = do
    let os_all = ossFlatten (specOpts spec)
    checkSpec spec os_all >> parseArgs0 spec os_all as (PSt [] 0 []) opts


-- options indices and number of arugments set
data PSt o = PSt {
     stOptsSet :: ![Int]
   , stArgIx :: !Int
   , stTriggers :: ![OptSpec o]
   } deriving (Show)


-- parsing loop; takes:
-- (1) spec
-- (2) the flattened option set
-- (3) the command line
-- (4) set-state (which options are are set, which arg index we are on)
-- (5) o: the user-defined options type
parseArgs0 :: Spec o -> [OptSpec o] -> [String] -> PSt o -> o -> IO o
parseArgs0 spec _ []     (PSt oset aix _) o0 = do
  -- parsing done, now set any derived options
  let opts = specOpts spec
      args = specArgs spec
      badArg = specBadArg spec
  let accUnsetOpt optlist o ix = do
        let os = optlist !! ix
        case osDerived os of
          Nothing
            | OptAttrAllowUnset `elem` osAttrs os -> return o
            | otherwise -> badArg (Just os) $ osFriendlyName os ++ " not set"
          Just derv -> derv o
  let notSet = [(0 :: Int) .. length opts - 1] \\ sort oset
  o1 <- foldM (accUnsetOpt opts) o0 notSet
  foldM (accUnsetOpt args) o1 [aix .. length args - 1]
parseArgs0 spec os_all (a:as) st o =
  case a of
   ('-':'-':larg) -> handleOpt osLong "--" larg
   ('-':sarg)     -> handleOpt osShort "-"  sarg
   _ -> handleArg a as
  where badArg = specBadArg spec

        unrecognizedArg k = badArg Nothing $ "unrecognized option " ++ k ++ hints_sfx
              where hints_sfx
                      | length k < 2 = ""
                      | otherwise =
                        case nub (similarSymbols k) of
                          [] -> ""
                          [s] -> "; did you mean " ++ s ++ "?"
                          [s1,s2] -> "; did you mean " ++ s1 ++ " or " ++ s2 ++ "?"
                          [s1,s2,s3] -> "; did you mean " ++ s1 ++ ", " ++ s2 ++ " or " ++ s3 ++ "?"
                          _ -> "" -- too many

                    similarSymbols :: String -> [String]
                    similarSymbols k = rmDash ++ addDash ++ typos k
                      where rmDash = filter (drop 1 k==) (short ++ long) :: [String]
                            addDash = filter (('-':k)==) (short ++ long) :: [String]

                            typos :: String -> [String]
                            -- take the top 3 and only those above 2/3 matching
                            typos k = map fst (takeWhile ((>=0.667) . snd) top3)
                              where allOpts = filter (not . null) (map osShort opts ++ map osLong opts) -- no - or -- prefixing (same for k)
                                    top3 = take 3 $ reverse $ sortBy (comparing snd) $ map (\o -> (o,similarity k o)) (long ++ short)

                            optSyms by pfx = map (pfx++) . filter (not . null) . map by $ opts
                            short = optSyms osShort "-"
                            long = optSyms osLong "--"

        opts = specOpts spec
        args = specArgs spec
        aix = stArgIx st

        shortOpts = filter (osHasAttr OptAttrAllowFusedSyntax) opts

        -- handleOpt :: (OptSpec o -> String) -> String -> String -> IO o
        handleOpt prj dashes opt_str =
          case span (/='=') opt_str of
            (k,('=':v)) -> handleUnry prj dashes k v as
            -- it's either a flag or fused option
            -- (e.g. "-j8" or "-Rs")
            (k,"")
              | any ((k==) . osShort) opts ->
                -- if we have: "-pTm0All",
                --    but "-pTm0" is also option, then we will favor flag interpretation
                --    "-pTm0All" over ""
                handleFlag prj dashes k as
            --  | any (`elem`(map osShort opts)) (inits k) ->
            --    -- if it's ambiguous, it's an error.
            --    -- e.g. given "-j10", if "-j1" and "-j" are options
            --      badArg Nothing ("short fused option " ++ dashes ++ k ++ " is ambiguous")
              | otherwise ->
                -- it doesn't map to any short option
                -- try and interpret it as fused short "-j32"
                -- we check all prefixes and ensure that it is not ambiguous
                -- e.g. "-j32" is ambiguous if -j and -j3 are options (could be -j=32 or -j3=2
                --      "-Xfoo" is ambiguous if -X is an argument and -Xfoo is an option
                case filter (\pfx -> pfx `elem` map osShort shortOpts) (tail (inits k)) of
                  -- we got an exact match
                  [k_fused] -> handleUnry prj dashes k_fused (drop (length k_fused) k) as
                  -- doesn't match anything; so punt (handleUnaryFlag raises a correct error message)
                  [] -> handleFlag prj dashes k as
                  -- the short option matches multiple possibilities
                  (k_fused1:k_fused2:_) -> badArg Nothing $ "fused option " ++ dashes ++ k ++ " is ambiguous: could be " ++ f k_fused1 ++ " or " ++ f k_fused2
                    where f k_f = dashes ++ k_f ++ "=" ++ drop (length k_f) k
        -- handleFlag :: (OptSpec o -> String) -> String -> String -> [String] -> IO o
        handleFlag prj dashes k as = do
          mos <- findOpSpec spec os_all (\os -> prj os == k) (stOptsSet st)
          case mos of
            Nothing -> unrecognizedArg (dashes ++ k)
            Just (ix,OptSpec _ _ _ _ _ _ _ (Just set_flag) _ _ _) ->
              -- It's a pure flag --foo
              set_flag o >>= parseArgs0 spec os_all as (st{stOptsSet = ix : stOptsSet st})
            Just (ix,os@(OptSpec _ _ _ _ _ _ _ Nothing (Just set_arg) _ _)) -> do
              -- It's in the form of: --foo arg
              if null as then
                badArg (Just os) $ dashes ++ k ++ " expects " ++ osTypeName os
                else handleUnry prj dashes k (head as) (tail as)
            Just (_,os)
              -- e.g. -X means show help on -X
              | osIsGroup os -> putStr (fmtOptSpecExtDesc spec os) >> exitSuccess
              | otherwise -> badArg (Just os) $ dashes ++ k ++ " does not expect argument"

        -- handleUnry :: (OptSpec o -> String) -> String -> String -> String -> [String] -> IO o
        handleUnry prj dashes k v as = do
          -- --fo
          mos <- findOpSpec spec os_all (\os -> prj os == k) (stOptsSet st)
          case mos of
            Nothing -> unrecognizedArg (dashes ++ k)
            Just (ix,os@(OptSpec _ _ _ _ _ _ _ _ (Just set_arg) _ _)) ->
              set_arg v o >>= parseArgs0 spec os_all as (st{stOptsSet = ix : stOptsSet st})
            Just (_,os) -> badArg (Just os) $ dashes ++ k ++ " expects " ++ osTypeName os
        -- handleArg :: String -> IO a
        handleArg astr as
          | null args = badArg Nothing $ astr ++ ": unexpected argument"
          | aix < length args = handleArgSpec (args !! aix) (aix + 1)
          | OptAttrAllowMultiple `elem` osAttrs (last args) = handleArgSpec (last args) (length args)
          | otherwise = badArg Nothing $ astr ++ ": too many arguments"
          where handleArgSpec a ix = -- ix is the next index (0-based for !!); also the 1-based index of this argument
                  case a of
                    OptSpec _ _ _ _ _ _ _ _ (Just set_arg) _ _ ->
                      set_arg astr o >>= parseArgs0 spec os_all as (st{ stArgIx = ix })
                    _ -> badSpec $ "argument " ++ show ix ++ " must have a valid osSetUnary"

ossFlatten :: [OptSpec o] -> [OptSpec o]
ossFlatten = go
  where go [] = []
        go (os:oss)
          | osIsGroup os = [os] ++ osMembers os ++ go oss
          | otherwise = os : go oss



similarity :: [Char] -> [Char] -> Float
similarity s1 s2 = fromIntegral (countMatching s1 s2) / len
  where len = fromIntegral (max (length s1) (length s2))
        countMatching [] _  = 0
        countMatching _  [] = 0
        countMatching (c1:cs1) (c2:cs2)
          | c1 == c2 = 1 + countMatching cs1 cs2
          | otherwise = countMatching cs1 cs2


findOpSpec :: Spec o -> [OptSpec o] -> (OptSpec o -> Bool) -> [Int] -> IO (Maybe (Int,OptSpec o))
findOpSpec spec@(Spec _ _ _ badArg _ _) os_all pr set = go os_all 0
  where go []     _ = return Nothing
        go (o:os) i
          | pr o = if not multi && i `elem` set then badArg (Just o) $ osFriendlyName o ++ " already set"
                    else return $ Just (i,o)
          | otherwise = go os (i+1)
          where multi = osHasAttr OptAttrAllowMultiple o

printUsage :: Spec o -> IO ()
printUsage spec = hPutStr stdout (fmtSpec spec) >> hFlush stdout


checkSpec :: Spec o -> [OptSpec o] -> IO ()
checkSpec spec opts = do
  let args = specArgs spec
      notNull = not . null
  -- forM_ opts $ \o -> do
  --  print o

  -- check each option
  forM_ (zip [(0 ::Int) ..] opts) $ \(ix,os) -> do
    when (any (=='=') (osShort os)) $
      badSpec $ "OptSpec #" ++ show ix ++ " (w/ type " ++ osTypeName os ++ ") short name contains an \'=\'"
    when (any (=='=') (osLong os)) $
      badSpec $ "OptSpec #" ++ show ix ++ " (w/ type " ++ osTypeName os ++ ") long name contains an \'=\'"
    when (null (osShort os) && null (osLong os)) $
      badSpec $ "OptSpec #" ++ show ix ++ " (w/ type " ++ osTypeName os ++ ") lacks a long or short name"

  -- check each arg
  forM_ (zip [(0 :: Int) ..] args) $ \(ix,as) -> do
    when (not (null (osShort as)) || not (null (osShort as))) $
      badSpec $ "ArgSpec #" ++ show ix ++ " arguments may not have option names"
  let -- ensureInitNotMulti :: Int -> OptSpec o -> IO ()
      ensureInitNotMulti _ [] = return ()
      ensureInitNotMulti ix (as:ass)
        | OptAttrAllowMultiple `elem` osAttrs as && notNull ass = badSpec $ "ArgSpec #" ++ show ix ++ " allows multiple specification; hence trailing ArgSpec's are unmatchable"
        | otherwise = ensureInitNotMulti ix ass
  ensureInitNotMulti (0 :: Int) args

  -- ensure unique names
  let nameSet = map osShort opts ++ map osLong opts
      -- don't count null strings: ""
      dups = filter (not . null) (nameSet \\ nub nameSet)
  when (not (null dups)) $
    badSpec $ "duplicated option names " ++ show dups

-- indicates an internal problem with the user's specification
badSpec :: String -> IO a
badSpec msg = hPutStrLnRed stderr ("Prog.Args.Impl: ARGUMENT SPEC DEFINITION ERROR\n" ++
                                    "NOTE: This indicates a problem with definition of progargs spec, not the command-line input to that spec.\n" ++
                                    msg) >> exitFailure

-------------------------------------------------------------------------------
--
-- IO
--
hPutStrRed, hPutStrLnRed :: Handle -> String -> IO ()
hPutStrRed h = hPutColored h SCA.Red SCA.Vivid
hPutStrLnRed h = hPutColoredLn h SCA.Red SCA.Vivid

hPutColored, hPutColoredLn :: Handle -> SCA.Color -> SCA.ColorIntensity -> String -> IO ()
hPutColored h c i s = do
  tty <- hIsTerminalDevice h
  finally (do
    when tty $ SCA.hSetSGR h [SCA.SetColor SCA.Foreground i c]
    hPutStr h s)
    (when tty $ SCA.hSetSGR h [SCA.Reset])
hPutColoredLn h c i s = hPutColored h c i (s ++ "\n")

