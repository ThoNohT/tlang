module CompilerFlag (
  CompilerFlag,
  BuildFlag (..),
  CleanFlag (..),
  allFlags,
  isActive,
  explain,
  fromString,
  accumulate,
  showFlags,
) where

import Core (rightPad)
import qualified Data.List as List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, lookup, toList)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import StrFmt

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

{- | Converts a list of argument strings into a set of compiler flags containg all flags specified in the list.
   If the list contains any invalid arguments, the list of invalid arguments is returned.
-}
accumulate :: CompilerFlag a => [String] -> Either [String] (Set a)
accumulate = foldl checkArg (Right Set.empty)
 where
  checkArg acc arg =
    case (fromString arg, acc) of
      (Just flag, Right flags) -> Right (Set.insert flag flags)
      (Nothing, Right _) -> Left [arg]
      (Nothing, Left invalidArgs) -> Left (arg : invalidArgs)
      _ -> acc

-- | Converts the list of flags into a string that can be displayed in the usage message.
showFlags :: CompilerFlag a => Map String a -> String
showFlags = unlines . fmap printFlag . Map.toList
 where
  printFlag (k, v) = sfmt ("        " % str % " " % str) (rightPad ' ' 14 k) (explain v)

-- | A compiler flag for the build command.
data BuildFlag
  = Run
  | DumpLexerTokens
  | DumpUncheckedSyntaxTree
  | DumpCheckedSyntaxTree
  | PrettyPrintAsm
  | UseNasm
  | NoColorBuild
  deriving (Eq, Ord)

instance CompilerFlag BuildFlag where
  allFlags =
    Map.fromList
      [ ("-r", Run)
      , ("-dt", DumpLexerTokens)
      , ("-dtu", DumpUncheckedSyntaxTree)
      , ("-dtc", DumpCheckedSyntaxTree)
      , ("-app", PrettyPrintAsm)
      , ("-nasm", UseNasm)
      , ("-nc", NoColorBuild)
      ]
  explain = \case
    Run -> "Run the program after compiling it."
    DumpLexerTokens -> "Dump the tokens produced by the lexer and exit."
    DumpUncheckedSyntaxTree -> "Dump the unchecked syntax tree produced by the parser and exit."
    DumpCheckedSyntaxTree -> "Dump the checked syntax tree produced by the checker and exit."
    PrettyPrintAsm -> "Pretty print assembly code, including comments and indentation."
    UseNasm -> "Use nasm rather than fasm for compiling."
    NoColorBuild -> "Don't use color when outputting data on the console."

-- | A compiler flag for the clean command.
data CleanFlag = IncludeExecutable | NoColorClean deriving (Eq, Ord)

instance CompilerFlag CleanFlag where
  allFlags = Map.fromList [("-e", IncludeExecutable), ("-nc", NoColorClean)]
  explain = \case
    IncludeExecutable -> "Also cleanup the compiled executable."
    NoColorClean -> "Don't use color when outputting data on the console."
