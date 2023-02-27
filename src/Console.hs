module Console (
  color,
  bold,
  ePutStrLn,
  faint,
  Formattable,
  format,
  formatBare,
  getFlagsOrExit,
  printUsage,
  testConditionWithUsageError,
  testCondition,
  assertJustWithUsageError,
  assertJust,
  assertRight,
  exitWithError,
  exitWithUsageError,
  runCmdEchoed,
) where

import CompilerFlag (BuildFlag, CleanFlag, CompilerFlag, allFlags, showFlags)
import Core (rightPad)
import Data.Foldable (Foldable (toList))
import Data.List qualified as List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, lookup, toList)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import GHC.IO.Handle.FD (stderr)
import StrFmt
import System.Exit (ExitCode (ExitFailure), exitFailure)
import System.IO (hPutStr, hPutStrLn)
import System.Process (readProcessWithExitCode, showCommandForUser)

{- | Applies a color coding around the specified string.
   https://chrisyeh96.github.io/2020/03/28/terminal-colors.html
-}
color :: Bool -> Int -> String -> String
color False _ elem = elem
color True nr elem = sfmt ("\x1b[" % int % "m" % str % "\x1b[0m") nr elem

-- | Makes specified string bold.
bold = flip color 1

-- | Makes the specified string look less intense.
faint = flip color 2

{- | Formattable objects can be easily formatted, and their formatted strings can be used in the formatting of other
   objects, while easily keeping track of the indentation needed to format everything in a nested manner.
-}
class Formattable a where
  -- | Create a formatted string for an object without being concerned with indentation.
  formatBare :: Bool -> a -> String

-- | Format an object using formatBare, and then applying the specified indentation.
format :: Formattable a => Bool -> Int -> a -> String
format useColor indent = List.intercalate "\n" . fmap indentLine . lines . formatBare useColor
 where
  indentLine l =
    if indent > 0
      then sfmt (" " % str % str) (faint useColor $ color useColor 36 ".") $ replicate (indent * 4 - 2) ' ' ++ l
      else l

instance (Foldable t, Functor t, Formattable a) => Formattable (t a) where
  formatBare useColor = List.intercalate "\n" . toList . fmap (formatBare useColor)

-- | Print the specified text to stderr.
ePutStrLn = hPutStrLn stderr

ePutStr = hPutStr stderr

{- | Gets the set of flags from an Either that contains the flags or the list of invalid flags.
   If there are invalid flags, an error message is printed to stderr and the program exits.
-}
getFlagsOrExit :: CompilerFlag a => String -> Either [String] (Set a) -> IO (Set a)
getFlagsOrExit compilerName = \case
  Left invalidFlags ->
    exitWithUsageError compilerName $
      sfmt
        ("Invalid flags: " % str)
        (List.intercalate ", " invalidFlags)
  Right flags -> pure flags

-- | Print the usage string.
printUsage compilerName = do
  putStrLn $ sfmt ("Usage: " % str % " <COMMAND> [OPTIONS]") compilerName
  putStrLn "  COMMAND:"
  putStrLn "    build <file>     Build the project specified in the specified file."
  putStrLn "      OPTIONS:"
  putStr $ showFlags (allFlags :: Map String BuildFlag)
  putStrLn "    clean <file>     Clean the output for the project specified in the specified file."
  putStrLn "      OPTIONS:"
  putStrLn $ showFlags (allFlags :: Map String CleanFlag)

{- | Checks a condition, and if it fails, shows the usage string and the specified error
   and then exits with exit code 1.
-}
testConditionWithUsageError :: String -> Bool -> String -> IO ()
testConditionWithUsageError compilerName False error = exitWithUsageError compilerName error
testConditionWithUsageError compilerName True error = pure ()

-- | Checks a condition, and if it fails, shows the specified error and then exits with exit code 1.
testCondition :: Bool -> String -> IO ()
testCondition False error = exitWithError error
testCondition True error = pure ()

{- | Unwraps a Maybe, and if it is Nothing, shows the usage string and the specified error
   and then exits with exit code 1.
-}
assertJustWithUsageError :: String -> String -> Maybe a -> IO a
assertJustWithUsageError compilerName error Nothing = exitWithUsageError compilerName error
assertJustWithUsageError _ _ (Just v) = pure v

-- | Unwraps a Maybe, and if it is Nothing, then shows the specified error and exits with exit code 1.
assertJust :: String -> Maybe a -> IO a
assertJust error Nothing = exitWithError error
assertJust _ (Just a) = pure a

-- | Unwraps an Either String, and if it is Left, then shows the specified error and exits with exit code 1.
assertRight :: Either String a -> IO a
assertRight (Left err) = exitWithError err
assertRight (Right a) = pure a

-- | Exits the application with exit code 1 after showing the specified error message.
exitWithError :: String -> IO a
exitWithError error = do
  ePutStrLn error
  exitFailure

-- | Exits the application with exit code 1 after showing the usage string and the specified error message.
exitWithUsageError :: String -> String -> IO a
exitWithUsageError compilerName error = do
  printUsage compilerName
  exitWithError error

{- | Runs a command and echoes the command to stdout.
   If the command fails, the output from stderr is returned and the program exits.
   Stdout output of the command is only displayed if requested.
-}
runCmdEchoed :: FilePath -> [String] -> Bool -> IO ()
runCmdEchoed path args echoStdOut = do
  let showCmd = showCommandForUser path args
  putStrLn $ sfmt ("[CMD] " % str) showCmd
  (exitCode, stdout, stderr) <- readProcessWithExitCode path args ""
  if echoStdOut then putStr stdout else pure ()
  case exitCode of
    ExitFailure code -> do
      ePutStr stderr
      exitWithError $ sfmt ("Command exited with status " % int % ".") code
    ExitSuccess -> pure ()
