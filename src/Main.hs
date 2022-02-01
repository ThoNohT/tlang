module Main where

import CompilerFlag (BuildFlag (..), CleanFlag (..))
import qualified CompilerFlag (accumulate, isActive)
import qualified Console (assertJustWithUsageError, assertRight, exitWithUsageError, formatBare, getFlagsOrExit, runCmdEchoed)
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.List as List (uncons)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import Lexer (lexFile)
import qualified Parser as P
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import Text.Printf (printf)

-- | All keywords in the language.
keywords :: Set Text
keywords = Set.fromList ["Executable", "let", "print", "return"]

-- | Compiles the project in the specified file. Returns the file path to the executable that was compiled.
compile :: FilePath -> Set BuildFlag -> IO FilePath
compile fileName flags = do
  input <- readFile fileName
  tokens <- Console.assertRight $ first T.unpack $ lexFile keywords fileName $ T.pack input

  if CompilerFlag.isActive DumpLexerTokens flags
    then do
      putStr $ unlines $ fmap Console.formatBare tokens
      exitSuccess
    else pure ()

  uncheckedProject <- Console.assertRight $ P.run P.projectParser tokens

  if CompilerFlag.isActive DumpUncheckedSyntaxTree flags
    then do
      putStrLn $ Console.formatBare uncheckedProject
      exitSuccess
    else pure ()

  undefined

-- | Cleans up the intermediary files created while compiling the program.
cleanup :: FilePath -> Set CleanFlag -> IO ()
cleanup = undefined

main :: IO ()
main = do
  compilerName <- getProgName
  args <- getArgs

  (cmd, args_) <- Console.assertJustWithUsageError compilerName "Command not specified." $ List.uncons args

  let cmd = head args
  let args_ = tail args
  case cmd of
    -- build Builds a project.
    "build" -> do
      (target, flagStrs) <- Console.assertJustWithUsageError compilerName "Build target not specified." $ List.uncons args_
      flags <- Console.getFlagsOrExit compilerName $ CompilerFlag.accumulate flagStrs
      exeFile <- compile target flags
      if CompilerFlag.isActive Run flags
        then Console.runCmdEchoed exeFile [] True
        else pure ()

    -- clean runs cleanup.
    "clean" -> do
      (target, flagStrs) <- Console.assertJustWithUsageError compilerName "Clean target not specified." $ List.uncons args_
      flags <- Console.getFlagsOrExit compilerName $ CompilerFlag.accumulate flagStrs
      cleanup target flags

    -- Any other command is invalid.
    _ -> Console.exitWithUsageError compilerName $ printf "Invalid command '%s'." cmd
  exitSuccess
