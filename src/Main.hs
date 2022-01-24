{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (foldM_)
import Data.Bifoldable (bifoldlM)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map (fromList, lookup, toList)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, insert, member)
import GHC.IO.Handle.Text (hPutStrLn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import Text.Printf (printf)

{- Prelude -}

-- | Adds padding of the specified character at the end of the string, until it has at least the specified width.
rightPad :: Char -> Int -> String -> String
rightPad padChar width input = input ++ replicate toPad ' '
  where
    toPad = max 0 $ width - length input

-- | Adds padding of the specified character at the start of the string, until it has at least the specified width.
leftPad :: Char -> Int -> String -> String
leftPad padChar width input = replicate toPad ' ' ++ input
  where
    toPad = max 0 $ width - length input

{- Console -}

-- | Applies a color coding around the specified string.
--   https://chrisyeh96.github.io/2020/03/28/terminal-colors.html
color :: Int -> String -> String
color nr elem = printf "%s%i%s%s%s" "\x1b[" nr "m" elem "\x1b[0m"

-- | Makes specified string bold.
bold = color 1

-- | Makes the specified string look less intense.
faint = color 2

-- | Formattable objects can be easily formatted, and their formatted strings can be used in the formatting of other
--   objects, while easily keeping track of the indentation needed to format everything in a nested manner.
class Formattable a where
  -- | Create a formatted string for an object without being concerned with indentation.
  formatBare :: a -> String

-- | Format an object using formatBare, and then applying the specified indentation.
format :: Formattable a => Int -> a -> String
format indent = unlines . map indentLine . lines . formatBare
  where
    indentLine l =
      if indent > 0
        then " ." ++ replicate ((indent * 4) - 2) ' ' ++ l
        else l

-- | Print the specified text to stderr.
ePutStrLn = hPutStrLn stderr . color 31

-- | A flag that can be used to define custom behavior for a command.
class Ord a => CompilerFlag a where
  -- | A map containing all flags, indexed by the argument string that triggers the flag.
  allFlags :: Map String a

  -- | Returns a string explaining the specified flag.
  explain :: a -> String

-- | Checks whether the flag is active in the provided set of flags.
isActive :: CompilerFlag a => a -> Set a -> Bool
isActive = Set.member

-- | Converts an argument string into a flag, if it is specified in allFlags.
fromString :: CompilerFlag a => String -> Maybe a
fromString str = Map.lookup str allFlags

-- | Converts a list of argument strings into a set of compiler flags containg all flags specified in the list.
--   If the list contains any invalid arguments, the list of invalid arguments is returned.
accumulate :: CompilerFlag a => [String] -> Either [String] (Set a)
accumulate = foldl checkArg (Right Set.empty)
  where
    checkArg acc arg =
      case (fromString arg, acc) of
        (Just flag, Right flags) -> Right (Set.insert flag flags)
        (Nothing, Left invalidArgs) -> Left (arg : invalidArgs)
        _ -> acc

-- | Converts the list of flags into a string that can be displayed in the usage message.
showFlags :: CompilerFlag a => Map String a -> String
showFlags = unlines . map printFlag . Map.toList
  where
    printFlag (k, v) = printf "        %s %s" (rightPad ' ' 14 k) (explain v)

-- | A compiler flag for the build command.
data BuildFlag
  = Run
  | DumpLexerTokens
  | DumpUncheckedSyntaxTree
  | DumpCheckedSyntaxTree
  | PrettyPrintAsm
  | UseNasm
  deriving (Eq, Ord)

instance CompilerFlag BuildFlag where
  allFlags =
    Map.fromList
      [ ("-r", Run),
        ("-dt", DumpLexerTokens),
        ("-dtu", DumpUncheckedSyntaxTree),
        ("-dtc", DumpCheckedSyntaxTree),
        ("-app", PrettyPrintAsm),
        ("-nasm", UseNasm)
      ]
  explain = \case
    Run -> "Run the program after compiling it."
    DumpLexerTokens -> "Dump the tokens produced by the lexer and exit."
    DumpUncheckedSyntaxTree -> "Dump the unchecked syntax tree produced by the parser and exit."
    DumpCheckedSyntaxTree -> "Dump the checked syntax tree produced by the checker and exit."
    PrettyPrintAsm -> "Pretty print assembly code, including comments and indentation."
    UseNasm -> "Use nasm rather than fasm for compiling."

-- | A compiler flag for the clean command.
data CleanFlag = IncludeExecutable deriving (Eq, Ord)

instance CompilerFlag CleanFlag where
  allFlags = Map.fromList [("-e", IncludeExecutable)]
  explain = \case
    IncludeExecutable -> "Also cleanup the compiled executable."

-- | Print the usage string.
printUsage compilerName = do
  putStrLn $ printf "Usage: %s <COMMAND> [OPTIONS]" compilerName
  putStrLn "  COMMAND:"
  putStrLn "    build <name>     Build the program with the specified name."
  putStrLn "      OPTIONS:"
  putStr $ showFlags (allFlags :: Map String BuildFlag)
  putStrLn "    clean <name>     Clean the output for the program with the specified name."
  putStrLn "      OPTIONS:"
  putStrLn $ showFlags (allFlags :: Map String CleanFlag)

-- | Checks a condition, and if it fails, prints the usage string, displays the specified error
--   and then exits with exit code 1.
testConditionWithUsageError :: String -> Bool -> String -> IO ()
testConditionWithUsageError compilerName condition error = do
  if condition
    then pure ()
    else do
      ePutStrLn error
      printUsage compilerName
      exitFailure

-- | Exits the application with exit code 1 after showing the specified error message.
exitWithError :: String -> IO ()
exitWithError error = do
  ePutStrLn error
  exitFailure

main :: IO ()
main = do
  compilerName <- getProgName
  args <- getArgs

  testConditionWithUsageError compilerName (length args >= 8) "Not enough arguments"
  putStrLn "Hello World!"
  exitSuccess
