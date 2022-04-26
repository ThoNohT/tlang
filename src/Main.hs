module Main where

import qualified Checker as C
import CompilerFlag (BuildFlag (..), CleanFlag (..))
import qualified CompilerFlag (accumulate, isActive)
import qualified Console (assertJustWithUsageError, assertRight, ePutStrLn, exitWithUsageError, formatBare, getFlagsOrExit, runCmdEchoed)
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.List as List (uncons)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import Lexer (lexFile)
import qualified Parser as P
import StrFmt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)

-- | All keywords in the language.
keywords :: Set Text
keywords = Set.fromList ["Executable", "let", "print", "return"]

-- | Compiles the project in the specified file. Returns the file path to the executable that was compiled.
compile :: FilePath -> Set BuildFlag -> IO FilePath
compile fileName flags = do
  -- Read file.
  input <- readFile fileName

  -- Run lexer.
  tokens <- Console.assertRight $ first T.unpack $ lexFile keywords fileName $ T.pack input

  let useColor = not $ CompilerFlag.isActive NoColor flags

  if CompilerFlag.isActive DumpLexerTokens flags
    then do
      putStr $ unlines $ fmap (Console.formatBare useColor) tokens
      exitSuccess
    else pure ()

  -- Run parser.
  uncheckedProject <- Console.assertRight $ P.run P.projectParser useColor tokens

  if CompilerFlag.isActive DumpUncheckedSyntaxTree flags
    then do
      putStrLn $ Console.formatBare useColor uncheckedProject
      exitSuccess
    else pure ()

  -- Run checker.
  case C.checkProject uncheckedProject of
    C.Failed issues -> do
      Console.ePutStrLn "Issues found:\n"
      Console.ePutStrLn $ Console.formatBare useColor issues
      exitFailure
    C.Checked issues project -> do
      if not $ null issues
        then do
          putStrLn "Issues found:\n"
          putStrLn $ Console.formatBare useColor issues
        else pure ()

      if CompilerFlag.isActive DumpCheckedSyntaxTree flags
        then do
          putStrLn $ Console.formatBare useColor project
          exitSuccess
        else pure ()

      -- TODO: Run compiler.
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
    _ -> Console.exitWithUsageError compilerName $ sfmt ("Invalid command '" % str % "'.") cmd
  exitSuccess
