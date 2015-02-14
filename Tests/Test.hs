module Main where

import Prog.Args.Args
import Prog.Args.Impl

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import System.IO

data Opts = Opts {
    oVerbose :: Bool
  , oThreads :: Int
  , oFile :: FilePath
  , oLogFile :: FilePath
  , oArgs :: [String]
  } deriving (Show, Eq)

dfltOpts :: Opts
dfltOpts = Opts False 1 "" "" []

spec :: Spec Opts
spec = mkSpecsWithHelpOpt "testargs" "tests Args package" 80 [
                  flag spec "v" "verbose"
                    "the verbosity level" ""
                    (\o -> (o {oVerbose = True}))

                , opt spec "j" "parallel-jobs" "INT"
                    "number of parallel jobs to execute"
                    "tests are built and executed with this level of parallelism"
                    (\i o -> o{oThreads = i}) `withDefault` (\o -> o{oThreads = 16})

                , opt spec "f" "file" "PATH"
                    "The file to read from" "This is the file to read from"
                    (\f o -> (o {oFile = f}))

                , opt spec "l" "log-file" "PATH"
                    "an optional logfile" "Yes, it's optional"
                    (\f o -> (o {oLogFile = f})) `withAttributes` [OptAttrAllowUnset]

          --      , trigger spec "t" "trigger"
          --          "triggers something" ""
          --          (\o -> print o >> return o)

                ]
                [
                  arg spec "ARG1" "the arg" "long arg desc" (\a o -> o{oArgs = oArgs o ++ [a]})
                , arg spec "ARG2" "the second arg" "the second arg" (\a o -> o{oArgs = oArgs o ++ [a]})
                ]

tArgs = dfltOpts { oFile = "foo.txt", oThreads = 16, oArgs = ["a","b"] }
merge as z = return (as,z)
negativeTest as = testBody (Left "ExitFailure 1") as >>= merge as
positiveTest as os = testBody (Right os) as >>= merge as
positiveExitSuccess as = testBody (Left "ExitSuccess") as >>= merge as
tests = [
    negativeTest []
  , positiveExitSuccess ["-h"]
  , positiveExitSuccess ["--help"]
  , positiveTest ["a","b","--file=foo.txt"] tArgs
  , positiveTest ["a","b","--file=foo.txt", "--log-file=log.txt"] tArgs{oLogFile="log.txt"}
  , negativeTest ["a","b","c","--file=foo.txt"]
  ]

-- testargs: option option --file not set
runAllTests :: IO ()
runAllTests = do
  rs <- sequence tests
  sequence $ replicate 24 (putStrLn "")
  if (any (not . snd) rs) then do
    putStrLn "FAILED:"
    forM_ rs $ \(as,z) -> do
      when (not z) $ do
        putStrLn $ "run " ++ show as
    else do
      putStrLn "ALL TESTS PASSED"

run = parseArgs spec dfltOpts

testBody :: Either String Opts -> [String] -> IO Bool
testBody ref args = do
  let handler :: SomeException -> IO (Either String Opts)
      handler e = return $ Left (show e)

      parse = Right <$> parseArgs spec dfltOpts args
  putStrLn ""
  putStrLn $ "  " ++ show args ++ " -> "
  let passed = putStrLn "ok" >> return True
      failed msg = hPutStrLnRed stdout msg >> return False
  act <- parse `catch` handler
  case (act,ref) of
    (Left aerr,Left rerr)
      | rerr `isInfixOf` aerr -> passed
      | otherwise -> failed $ "error mismatch: " ++ aerr
    (Left aerr,_) -> failed $ "unexpected error: " ++ show aerr
    (Right _,Left _) -> failed $ "unexpected pass"
    (Right aopts, Right ropts)
      | aopts /= ropts -> failed $ "parsed wrong arguments:\n" ++
                                     "GOT:      " ++ show aopts ++ "\n" ++
                                     "EXPECTED: " ++ show ropts
      | otherwise -> passed
