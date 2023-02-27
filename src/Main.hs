module Main where

import qualified Checker as Ch
import qualified Compiler as C
import CompilerFlag (BuildFlag (..), CleanFlag (..))
import qualified CompilerFlag (accumulate, isActive)
import qualified Console (
  assertJustWithUsageError,
  assertRight,
  ePutStrLn,
  exitWithUsageError,
  formatBare,
  getFlagsOrExit,
  runCmdEchoed,
 )
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.List as List (uncons)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import Lexer (lexFile)
import qualified Parser as P
import Project (Project (Project), ProjectType (..), UncheckedProject (UncheckedProject))
import StrFmt
import System.Directory (doesFileExist, removeFile)
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

  let useColor = not $ CompilerFlag.isActive NoColorBuild flags

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
  case Ch.checkProject uncheckedProject of
    Ch.Failed issues -> do
      Console.ePutStrLn "Issues found:\n"
      Console.ePutStrLn $ Console.formatBare useColor issues
      exitFailure
    Ch.Checked issues project@(Project Executable {name = projectName} program) -> do
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

      let asmFile :: FilePath = sfmt ("./" % txt % ".asm") projectName
      let oFile :: FilePath = sfmt ("./" % txt % ".o") projectName
      let exeFile :: FilePath = sfmt ("./" % txt) projectName

      putStrLn $ sfmt ("Generating " % str) asmFile
      C.writeX86_64_LinuxAsm asmFile program flags

      if CompilerFlag.isActive UseNasm flags
        then do
          -- Compile nasm.
          Console.runCmdEchoed "nasm" ["-f", "elf64", "-o", oFile, asmFile] True
          -- Link file.
          Console.runCmdEchoed "ld" [oFile, "-o", exeFile] True
        else do
          -- Compile fasm
          Console.runCmdEchoed "fasm" ["-m", "524288", asmFile, exeFile] True
          Console.runCmdEchoed "chmod" ["+x", exeFile] True

      pure exeFile

-- | Cleans up the intermediary files created while compiling the program.
cleanup :: FilePath -> Set CleanFlag -> IO ()
cleanup fileName flags = do
  -- Read file.
  input <- readFile fileName

  -- Run lexer.
  tokens <- Console.assertRight $ first T.unpack $ lexFile keywords fileName $ T.pack input

  -- Run parser.
  let useColor = not $ CompilerFlag.isActive NoColorClean flags
  UncheckedProject Executable {name = projectName} _ <- Console.assertRight $ P.run P.projectParser useColor tokens

  -- Run cleanup.
  putStrLn $ sfmt ("Cleaning up files for " % txt) projectName

  let asmFile :: FilePath = sfmt (txt % ".asm") projectName
  asmFileExists <- doesFileExist asmFile
  if asmFileExists
    then do
      putStrLn $ sfmt ("Removing " % str) asmFile
      removeFile asmFile
    else pure ()

  let oFile :: FilePath = sfmt (txt % ".o") projectName
  oFileExists <- doesFileExist oFile
  if oFileExists
    then do
      putStrLn $ sfmt ("Removing " % str) oFile
      removeFile oFile
    else pure ()

  let exeFile :: FilePath = sfmt txt projectName
  exeFileExists <- doesFileExist exeFile
  if exeFileExists && CompilerFlag.isActive IncludeExecutable flags
    then do
      putStrLn $ sfmt ("Removing " % str) exeFile
      removeFile exeFile
    else pure ()

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
