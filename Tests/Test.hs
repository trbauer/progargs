module Main where

import Prog.Args.Args
import Prog.Args.Impl

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import System.IO
import System.Exit

data Opts = Opts {
    oVerbosity :: Int
  , oThreads :: Int
  , oCoeff :: Double
  , oFile :: FilePath
  , oLogFile :: FilePath
  , oArgs :: [String]
  , oTriggers :: Int
  } deriving (Show, Eq)

dfltOpts :: Opts
dfltOpts = Opts 0 8 0 "" "" [] 0

goodSpec1 :: Spec Opts
goodSpec1 = mkSpecWithHelpOpt "testargs" "tests Args package" 0 [
                  optF goodSpec1 "v" "verbose"
                    "the verbosity level" ""
                    (\o -> (o {oVerbosity = 1}))

                , opt goodSpec1 "j" "parallel-jobs" "INT"
                    "number of parallel jobs to execute"
                    "tests are built and executed with this level of parallelism"
                    (\i o -> o{oThreads = i})
                      #= (\o -> o{oThreads = oThreads dfltOpts})
                      # OptAttrAllowFusedSyntax

                , opt goodSpec1 "f" "file" "PATH"
                    "The file to read from" "This is the file to read from"
                    (\f o -> (o {oFile = f}))

                , opt goodSpec1 "c" "coeff" "DBL"
                    "A special filter value" "Coefficient"
                    (\c o -> (o {oCoeff = c})) #++ [OptAttrAllowUnset, OptAttrAllowFusedSyntax]

                , opt goodSpec1 "l" "log-file" "PATH"
                    "an optional logfile" "Yes, it's optional"
                    (\f o -> (o {oLogFile = f})) # OptAttrAllowUnset

                , optFIO goodSpec1 "t" "trigger"
                    "triggers something" ""
                    (\o -> return o{oTriggers = oTriggers o + 1}) # OptAttrAllowMultiple

                , optG goodSpec1 "X"
                    "experimental options" "" [
                      -- -Xd
                      optF goodSpec1 "d" ""
                        "the verbosity level" ""
                        (\o -> (o {oVerbosity = 2}))
                      -- --Xquiet
                    , optF goodSpec1 "" "quiet"
                        "the verbosity level" ""
                        (\o -> (o {oVerbosity = -1}))
                      -- -Xv=... and -Xverbosity=...
                    , opt goodSpec1 "v" "verbosity" "INT"
                        "manually set verbosity" "manually set verbosity"
                        (\a o -> (o {oVerbosity = a})) # OptAttrAllowUnset
                    ]
                ]
                [
                  arg goodSpec1 "ARG1" "the arg" "long arg desc"
                    (\a o -> o{oArgs = oArgs o ++ [a]})
                , arg goodSpec1 "ARG2" "the second arg" "the second arg"
                    (\a o -> o{oArgs = oArgs o ++ [a]})
                ]
goodSpec2 :: Spec Opts
goodSpec2 =
  mkSpecWithHelpOpt "testargs" "tests Args package" 0 [][
                  arg goodSpec2 "ARG1" "the arg" "long arg desc"
                    (\a o -> o{oArgs = oArgs o ++ [a]})
                , arg goodSpec2 "ARG2" "the second arg" "the second arg"
                    (\a o -> o{oArgs = oArgs o ++ [a]}) # OptAttrAllowUnset
                ]
goodSpec3 :: Spec Opts
goodSpec3 =
  mkSpecWithHelpOpt "testargs" "tests Args package" 0 [][
                  arg goodSpec3 "ARG1" "the arg" "long arg desc"
                    (\a o -> o{oArgs = oArgs o ++ [a]})
                , arg goodSpec3 "ARG2" "the second arg" "the second arg"
                    (\a o -> o{oArgs = oArgs o ++ [a]}) #<- \o -> return o{oArgs = oArgs o ++ ["DEFAULT"]}
                ]


badSpec1 :: Spec Opts
badSpec1 =
  mkSpecWithHelpOpt "badSpec1" "tests Args package: badSpec1" 0 [
      optF badSpec1 "X" "oops"
        "conflicts" ""
        (\o -> (o {oVerbosity = 1})) # OptAttrAllowUnset
        , optG badSpec1 "X"
            "experimental options" "" [
              -- -Xd
              optF badSpec1 "d" ""
                "the verbosity level" ""
                (\o -> (o {oVerbosity = 2})) # OptAttrAllowUnset
              -- --Xquiet
            , optF badSpec1 "" "quiet"
                "the verbosity level" ""
                (\o -> (o {oVerbosity = -1})) # OptAttrAllowUnset
              -- -Xop
            , opt badSpec1 "op" "override-path" "PATH"
                "an optional logfile" "Yes, it's optional"
                (\f o -> (o {oLogFile = "<override>/" ++ f})) # OptAttrAllowUnset
            ]
  ]
  [
  ]

badSpec2 :: Spec Opts
badSpec2 =
  mkSpecWithHelpOpt "testargs2" "tests Args package: badSpec2" 0 [
      optF badSpec2 "Xfoo" "oops"
        "conflicts" ""
        (\o -> (o {oVerbosity = 1}))
        , optG badSpec2 "X"
            "experimental options" "" [
              optF badSpec2 "foo" ""
                "the verbosity level" ""
                (\o -> (o {oVerbosity = 2}))
            ]
  ]
  [
  ]

merge as z = return (show as,z)
mergeS s z = return (s,z)

positiveEq s as os = testBody s (Right os) as >>= merge as
positiveExitSuccess s as = testBody s (Left "ExitSuccess") as >>= merge as
negativeExits1 s as = testBody s (Left "ExitFailure 1") as >>= merge as
negativeSpecErr s nm = testNegativeSpecErrorBody s nm >>= mergeS ("<" ++ nm ++ ">")

tests :: [IO (String,Bool)]
tests = [
    negativeExits1 goodSpec1 []
  , positiveExitSuccess goodSpec1 ["-h"]
  , positiveExitSuccess goodSpec1 ["--help"]
  , positiveEq goodSpec1 ["a","b","--file=foo.txt"] tArgs
  , positiveEq goodSpec1 ["a","b","--file=foo.txt", "--log-file=log.txt"] tArgs{oLogFile="log.txt"}
  , positiveEq goodSpec1 ["a","b","--trigger","--file=foo.txt","-t"] tArgs{oTriggers = 2}
  , positiveEq goodSpec1 ["a","b","-j=2","--file=foo.txt"] tArgs{oThreads = 2}
  , positiveEq goodSpec1 ["a","b","-j2","--file=foo.txt"] tArgs{oThreads = 2}
  , positiveEq goodSpec1 ["a","b","-j2k","--file=foo.txt"] tArgs{oThreads = 2*1024}
  , positiveEq goodSpec1 ["a","b","-c3.141","--file=foo.txt"] tArgs{oCoeff = 3.141}
  , positiveEq goodSpec1 ["a","b","-c3.141M","--file=foo.txt"] tArgs{oCoeff = 3.141*1024*1024}
  , negativeExits1 goodSpec1 ["a","b","c","--file=foo.txt"]
  , positiveExitSuccess goodSpec1 ["-X"]
  , positiveExitSuccess goodSpec1 ["-h=X"]
  , positiveExitSuccess goodSpec1 ["a","b","--file=foo.txt","-X"]
  -- groups
  , positiveEq goodSpec1 ["a","b","--Xquiet","--file=foo.txt"] tArgs{oVerbosity = -1}
  , positiveEq goodSpec1 ["a","b","-Xv=14","--file=foo.txt"] tArgs{oVerbosity = 14}
  , positiveEq goodSpec1 ["a","b","-Xv=-15","--file=foo.txt"] tArgs{oVerbosity = -15}
  , positiveEq goodSpec1 ["a","b","--Xverbosity=16","--file=foo.txt"] tArgs{oVerbosity = 16}
  -- defaults
  , positiveEq goodSpec2 ["a","b"] (tArgsW ["a","b"])       -- normal
  , positiveEq goodSpec2 ["a"] (tArgsW ["a"])               -- default
  , positiveEq goodSpec3 ["a","b"] (tArgsW ["a","b"])       -- normal
  , positiveEq goodSpec3 ["a"] (tArgsW ["a","DEFAULT"]) -- default
  -- bad specs
  , negativeSpecErr badSpec1 "overlap opt and group"
  , negativeSpecErr badSpec2 "overlap -Xfoo"
  ]
tArgs = dfltOpts {oFile = "foo.txt", oArgs = ["a","b"]}
tArgsW as = dfltOpts {oArgs = as}




main :: IO ()
main = runAllTests


-- for interactive testing
run :: [String] -> IO ()
run = (>>= print) . parseArgs goodSpec1 dfltOpts


runAllTests :: IO ()
runAllTests = do
  rs <- sequence tests
  sequence $ replicate 24 (putStrLn "")
  if (any (not . snd) rs) then do
    putStrLn "FAILED:"
    forM_ rs $ \(as,z) -> do
      when (not z) $ do
        putStrLn $ "run " ++ show as
    exitFailure
    else do
      putStrLn "ALL TESTS PASSED"
      exitSuccess



testBody :: Spec Opts -> Either String Opts -> [String] -> IO Bool
testBody spec ref args = do
  let handler :: SomeException -> IO (Either String Opts)
      handler e = return $ Left (show e)

      parse = Right <$> parseArgs spec dfltOpts args
  putStrLn ""
  putStrLn $ "  " ++ show args ++ " -> "
  let passed = putStrLn "ok" >> return True
      failed msg = hPutStrLnRed stdout ("FAILED: " ++ msg) >> return False
  act <- parse `catch` handler
  case (act,ref) of
    (Left aerr,Left rerr)
      | rerr `isInfixOf` aerr -> passed
      | otherwise -> failed $ "mismatch: " ++ aerr
    (Left aerr,_) -> failed $ "unexpected error: " ++ show aerr
    (Right _,Left _) -> failed $ "unexpected pass"
    (Right aopts, Right ropts)
      | aopts /= ropts -> failed $ "parsed wrong arguments:\n" ++
                                     "GOT:      " ++ show aopts ++ "\n" ++
                                     "EXPECTED: " ++ show ropts
      | otherwise -> passed


testNegativeSpecErrorBody :: Spec Opts -> String -> IO Bool
testNegativeSpecErrorBody spec name = do
  let passed = putStrLn "ok" >> return True
      failed msg = hPutStrLnRed stdout ("FAILED: " ++ msg) >> return False
  putStrLn $ "  " ++ show name ++ " -> "
  let handler :: SomeException -> IO Bool
      handler e
        | "ExitFailure 1" `isInfixOf` show e = passed
        | otherwise = failed $ "wrong error: " ++ show e
  let test = do
        a <- parseArgs spec dfltOpts []
        print a
        failed "parseArgs didn't exit with an error"
  test `catch` handler