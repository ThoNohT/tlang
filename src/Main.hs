{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (foldM_)
import Control.Monad.Loops (whileM, whileM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as ST
import Data.Char as Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable (forM_)
import Data.Function ((&))
import qualified Data.List as List (intercalate, uncons)
import Data.Map (Map, (!?))
import qualified Data.Map as Map (fromList, lookup, toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, insert, member)
import Debug.Trace (trace, traceStack)
import GHC.IO.Exception (ExitCode (..))
import GHC.IO.Handle.Text (hPutStrLn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.Printf (printf)
import Text.Read (Lexeme (Char), readMaybe)

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

-- | Removes the specified character from a string.
removeChar :: Char -> String -> String
removeChar toRemove [] = []
removeChar toRemove (c : cs) | c == toRemove = removeChar toRemove cs
removeChar toRemove (c : cs) = c : removeChar toRemove cs

-- | whileM with the first argument applied to the state.
whileS a = whileM (a <$> ST.get)

-- | whileM with the first argument applied to the state, and lifted.
whileSE a = whileM (lift $ a <$> ST.get)

-- | whileM_ with the first argument applied to the state.
whileS_ a = whileM_ (a <$> ST.get)

-- | whileM_ with the first argument applied to the state, and lifted.
whileSE_ a = whileM_ (lift $ a <$> ST.get)

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
format indent = unlines . fmap indentLine . lines . formatBare
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
        (Nothing, Right _) -> Left [arg]
        (Nothing, Left invalidArgs) -> Left (arg : invalidArgs)
        _ -> acc

-- | Gets the set of flags from an Either that contains the flags or the list of invalid flags.
--   If there are invalid flags, an error message is printed to stderr and the program exits.
getFlagsOrExit :: CompilerFlag a => String -> Either [String] (Set a) -> IO (Set a)
getFlagsOrExit compilerName = \case
  Left invalidFlags -> exitWithUsageError compilerName $ printf "Invalid flags: %s" (List.intercalate ", " invalidFlags)
  Right flags -> pure flags

-- | Converts the list of flags into a string that can be displayed in the usage message.
showFlags :: CompilerFlag a => Map String a -> String
showFlags = unlines . fmap printFlag . Map.toList
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
  putStrLn "    build <file>     Build the project specified in the specified file."
  putStrLn "      OPTIONS:"
  putStr $ showFlags (allFlags :: Map String BuildFlag)
  putStrLn "    clean <file>     Clean the output for the project specified in the specified file."
  putStrLn "      OPTIONS:"
  putStrLn $ showFlags (allFlags :: Map String CleanFlag)

-- | Checks a condition, and if it fails, shows the usage string and the specified error
--   and then exits with exit code 1.
testConditionWithUsageError :: String -> Bool -> String -> IO ()
testConditionWithUsageError compilerName False error = exitWithUsageError compilerName error
testConditionWithUsageError compilerName True error = pure ()

-- | Checks a condition, and if it fails, shows the specified error and then exits with exit code 1.
testCondition :: Bool -> String -> IO ()
testCondition False error = exitWithError error
testCondition True error = pure ()

-- | Unwraps a Maybe, and if it is Nothing, shows the usage string and the specified error
--   and then exits with exit code 1.
assertJustWithUsageError :: String -> String -> Maybe a -> IO a
assertJustWithUsageError compilerName error Nothing = exitWithUsageError compilerName error
assertJustWithUsageError _ _ (Just v) = pure v

-- | Unwraps a Maybe, and if it is Nothing, then shows the specified error and exits with exit code 1.
assertJust :: String -> Maybe a -> IO a
assertJust error Nothing = exitWithError error
assertJust _ (Just a) = pure a

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

-- | Runs a command and echoes the command to stdout.
--   If the command fails, the output from stderr is returned and the program exits.
--   Stdout output of the command is only displayed if requested.
runCmdEchoed :: FilePath -> [String] -> Bool -> IO ()
runCmdEchoed path args echoStdOut = do
  let showCmd = showCommandForUser path args
  putStrLn $ printf "[CMD] %s" showCmd
  (exitCode, stdout, stderr) <- readProcessWithExitCode path args ""
  if echoStdOut then putStrLn stdout else pure ()
  case exitCode of
    ExitFailure code -> do
      ePutStrLn stderr
      exitWithError $ printf "Command exited with status %i." code
    ExitSuccess -> pure ()

{- Lexer -}

-- | For this lexer, we need to often match against whitespace characters that are not the newline character.
isWhitespace c = Char.isSpace c && c /= '\n'

-- | A position in a source file.
data Position = Position {line :: Int, col :: Int}

instance Formattable Position where
  formatBare Position {line, col} = printf "%i:%i" line col

-- | Converts a position to a string including the specified filename.
posToFileString :: FilePath -> Position -> String
posToFileString filename Position {line, col} = printf "%s:%i:%i" filename line col

-- | Initial position.
zeroPos :: Position
zeroPos = Position {line = 0, col = 0}

-- | Advances the position to the next column.
nextCol :: Position -> Position
nextCol pos = pos {col = col pos + 1}

-- | Advances the position to the next line.
nextLine :: Position -> Position
nextLine pos = pos {col = 0, line = line pos + 1}

-- | A range of positions in a source file.
data Range = Range {file :: FilePath, startPos :: Position, endPos :: Position}

instance Formattable Range where
  formatBare Range {file, startPos, endPos} =
    color 32 $ printf "[%s:%s->%s]" file (formatBare startPos) (formatBare endPos)

rangeFromPositions filename startPos endPos =
  Range {file = filename, startPos, endPos}

-- | Encodes all the differnt types of tokens, with their data.
data TokenData
  = IndentationToken Int
  | KeywordToken String
  | IdentifierToken String
  | SymbolToken String
  | StringLiteralToken String
  | NumberToken Int
  | SeparatorToken String
  | EndOfLineToken
  | EndOfInputToken
  | CommentToken String -- TODO: This tokens should not be consumed by the parser, but may be useful for reconstructing the original source code?
  deriving (Show)

instance Formattable TokenData where formatBare = bold . show

data Token = Token
  { range :: Range,
    tData :: TokenData,
    whitespaceBefore :: Maybe String
  }

instance Formattable Token where
  formatBare Token {range, tData, whitespaceBefore} =
    printf "%s %s\nwhitespaceBefore: %s\n%s" (formatBare range) (bold "Token") (color 35 $ show $ null whitespaceBefore) (format 1 tData)

data LexerState = LexerState
  { filename :: FilePath,
    input :: String,
    -- Positions
    pos :: Position,
    prevPos :: Position,
    backupPos :: Position,
    -- Output
    tokens :: [Token],
    -- Other state.
    accumulator :: [Char],
    autoAccumulate :: Bool,
    curChar :: Char,
    startOfLine :: Bool,
    spacesPerIndent :: Maybe Int,
    whitespaceBefore :: Maybe String,
    atEndOfInput :: Bool
  }

-- | Creates a lexer state from the specified input and filename.
createLexerState :: String -> FilePath -> LexerState
createLexerState input filename =
  LexerState
    { filename,
      input = sanitizedInput,
      pos = zeroPos,
      prevPos = zeroPos,
      backupPos = zeroPos,
      tokens = [],
      accumulator = [],
      autoAccumulate = True,
      curChar = firstChar,
      startOfLine = True,
      spacesPerIndent = Nothing,
      whitespaceBefore = Nothing,
      atEndOfInput
    }
  where
    (atEndOfInput, (firstChar, sanitizedInput)) =
      maybe (True, ('\0', [])) (False,) $ List.uncons $ removeChar '\r' input

-- | Moves to the next character in the lexer state.
--   If autoAccumulate is enabled then the tokens will be added to accumulator, until the end of the input is reached.
--   then the current char will be null, but no characters are accumulated.
nextChar :: LexerState -> LexerState
nextChar state =
  let updatePosition state = case curChar state of
        '\n' -> state {pos = nextLine $ pos state, prevPos = pos state}
        _ -> state {pos = nextCol $ pos state, prevPos = pos state}
      setNextChar state = case List.uncons $ input state of
        Just (nextChar, input') ->
          let newState = state {curChar = nextChar, input = input'}
           in if autoAccumulate state then accumulateChar nextChar newState else newState
        Nothing -> state {atEndOfInput = True, curChar = '\0'}
   in if atEndOfInput state
        then state
        else setNextChar $ updatePosition state

-- | Sets the token start position to the current position.
--   This position will be the start position for the next token added to the output.
setTokenStartPoint :: LexerState -> LexerState
setTokenStartPoint state = state {backupPos = pos state, accumulator = []}

-- | Sets the whitespaceBefore field to the specified value.
setWhitespaceBefore :: Maybe String -> LexerState -> LexerState
setWhitespaceBefore value state = state {whitespaceBefore = value}

-- | Adds a token to the output list.
--   Uses the state's backup position as the start of the token, and the previous position as the end.
--   whitespaceBefore is reset to Nothing.
addToken :: TokenData -> LexerState -> LexerState
addToken tData state@LexerState {tokens, whitespaceBefore, filename, backupPos, prevPos} =
  state {tokens = newToken : tokens, whitespaceBefore = Nothing}
  where
    newToken =
      Token {range = rangeFromPositions filename backupPos prevPos, whitespaceBefore, tData}

-- | Returns the accumulated string since the last start point.
accumulatedString :: LexerState -> String
accumulatedString LexerState {accumulator} = reverse accumulator

-- | Sets autoAccumulate to the specified value.
setAutoAccumulate :: Bool -> LexerState -> LexerState
setAutoAccumulate val state = state {autoAccumulate = val}

-- | Adds a character to the accumulator.
accumulateChar :: Char -> LexerState -> LexerState
accumulateChar val state@LexerState {accumulator} = state {accumulator = val : accumulator}

-- | Except transformer with State for LexerState and String error.
type LexerM a = ExceptT String (State LexerState) a

-- | Can be used to check a predicate based on the current state, and if it fails, raise an exception,
--   including some location data.
checkLexerPredicate :: (LexerState -> Bool) -> String -> LexerM ()
checkLexerPredicate pred msg = do
  s <- lift ST.get
  if pred s
    then pure ()
    else throwE $ printf "%s: Lexer error: %s" (posToFileString (filename s) (pos s)) msg

-- | Can be used to check a predicate, and if it fails, raise an exception, including some location data.
checkLexerPredicate' :: Bool -> String -> LexerM ()
checkLexerPredicate' True msg = pure ()
checkLexerPredicate' False msg = do
  s <- lift ST.get
  throwE $ printf "%s: Lexer error: %s" (posToFileString (filename s) (pos s)) msg

-- | Can be used to check that a Maybe is Just, and if it fails, raise an exception, including some location data.
lexerAssertJust :: Maybe a -> String -> LexerM a
lexerAssertJust (Just val) msg = pure val
lexerAssertJust Nothing msg = do
  s <- lift ST.get
  throwE $ printf "%s: Lexer error: %s" (posToFileString (filename s) (pos s)) msg

-- | Lexes an indentation token, consisting of spaces at the sart of a line.
--   Only allows spaces as indentation. The lexer will fail when it encounters any other whitespace character,
--   or when an unexpected number of spaces is encountered.
lexIndent :: LexerM ()
lexIndent = do
  lift $ ST.modify setTokenStartPoint
  spi <- lift $ ST.gets spacesPerIndent
  case spi of
    Nothing -> lift $ do
      -- The number of spaces per indent is not yet known, the total number of spaces encountered during the first time
      -- leading whitespace occurs is taken as the number of spaces per indent.
      whileS_
        (\s -> not (atEndOfInput s) && isWhitespace (curChar s))
        ( do
            ST.modify (\s -> s {spacesPerIndent = Just $ maybe 1 (1 +) (spacesPerIndent s)})
            ST.modify nextChar
        )

      ST.modify $ addToken $ IndentationToken 1
    Just spi' -> do
      -- The number of spaces per indent is known, so take a multiple of this number of spaces, and return this as
      -- the indent level.
      spaces <-
        whileSE
          (\s -> not (atEndOfInput s) && isWhitespace (curChar s))
          (checkLexerPredicate ((==) ' ' . curChar) "Leading whitespace may only consist of whitespaces.")
      let nSpaces = length spaces
      let sOffset = nSpaces `rem` spi'
      let prefix = "Invalid number of leading spaces. Expected a multiple of"
      checkLexerPredicate' (sOffset == 0) $
        printf "%s %i, but got %i, which %i too many or %i too few." prefix spi' nSpaces sOffset (spi' - sOffset)
      lift $ ST.modify $ addToken $ IndentationToken (nSpaces `div` spi')

-- | Lexes a number, simply a token with a value as long as the characters are numeric.
lexNumber :: LexerM ()
lexNumber = do
  lift $ ST.modify setTokenStartPoint
  lift $ whileS_ (\s -> not (atEndOfInput s) && isDigit (curChar s)) (ST.modify nextChar)
  nrStr <- lift $ ST.gets accumulatedString
  let nrMaybe = readMaybe nrStr :: Maybe Int
  nr <- lexerAssertJust nrMaybe "Failed to parse a number."
  lift $ ST.modify $ addToken $ NumberToken nr

-- | Lexes an identifier or a keyword, consumes characters as long as they are alphanumeric. If the resulting name is
--    contained in the set of keywords, a keyword token is added, otherwise an identifier token is added.
lexIdentifier :: Set String -> LexerM ()
lexIdentifier keywords = lift $ do
  ST.modify setTokenStartPoint
  whileS_ (\s -> not (atEndOfInput s) && isAlphaNum (curChar s)) (ST.modify nextChar)
  id <- ST.gets accumulatedString
  ST.modify $ addToken $ if Set.member id keywords then KeywordToken id else IdentifierToken id

-- | Lexes a string literal.
lexStringLiteral :: LexerM ()
lexStringLiteral = do
  lift $ ST.modify setTokenStartPoint
  lift $ ST.modify $ setAutoAccumulate False

  lift $ ST.modify nextChar
  whileSE_
    (\s -> not (atEndOfInput s) && curChar s /= '"')
    ( do
        curChar <- lift $ ST.gets curChar
        if curChar == '\\'
          then do
            escapedStr <- lexEscapedChar
            lift $ forM_ escapedStr (ST.modify . accumulateChar)
          else lift $ ST.modify $ accumulateChar curChar
    )

  assertNotAtEnd ""
  lift $ ST.modify nextChar
  str <- lift $ ST.gets accumulatedString
  lift $ ST.modify $ addToken $ StringLiteralToken str

  lift $ ST.modify $ setAutoAccumulate True
  where
    mappedChars :: Map Char Char
    mappedChars =
      Map.fromList [('\\', '\\'), ('/', '/'), ('b', '\x08'), ('f', '\x0c'), ('n', '\n'), ('r', '\r'), ('t', '\t')]

    lexEscapedChar :: LexerM String
    lexEscapedChar = do
      lift $ ST.modify nextChar
      assertNotAtEnd "escaped character in "
      curChar <- lift $ ST.gets curChar
      pure $ case mappedChars !? curChar of
        Just mc -> [mc]
        Nothing -> ['\\', curChar]

    assertNotAtEnd subject =
      checkLexerPredicate
        (not . atEndOfInput)
        $ printf "Input ended before %sstring literal ended." subject

-- | Lexes a symbol, or any other token that can be started by regular symbol characters.
lexSymbol :: LexerM ()
lexSymbol = lift $ do
  ST.modify setTokenStartPoint
  firstChar <- ST.gets curChar
  ST.modify nextChar
  secondChar <- ST.gets curChar
  case (firstChar, secondChar) of
    ('-', '-') -> do
      -- More than one - indicates we are at a separator character.
      whileS_ (\s -> not (atEndOfInput s) && curChar s == '-') (ST.modify nextChar)
      sep <- ST.gets accumulatedString
      ST.modify $ addToken $ SeparatorToken sep
    ('-', _) -> lexRegularSymbol
    ('/', '/') -> do
      -- Two / indicate a single-line comment. Just collect until a newline is consumed.
      whileS_ (\s -> not (atEndOfInput s) && curChar s /= '\n') (ST.modify nextChar)
      comment <- ST.gets accumulatedString
      ST.modify $ addToken $ CommentToken comment
      -- Move on to the next line.
      ST.modify nextChar
    _ -> lexRegularSymbol
  where
    isSymbolChar c = not $ isWhitespace c || isAlphaNum c

    lexRegularSymbol :: State LexerState ()
    lexRegularSymbol = do
      whileS_ (\s -> not (atEndOfInput s) && isSymbolChar (curChar s)) (ST.modify nextChar)
      sym <- ST.gets accumulatedString
      if not $ null sym then ST.modify $ addToken $ SymbolToken sym else pure ()

-- | Lexes some whitespace, sets whitespaceBefore to the whitespace that was parsed.
lexWhitespace :: LexerM ()
lexWhitespace = lift $ do
  whileS_ consumeMoreWhitespace (ST.modify nextChar)
  accStr <- ST.gets accumulatedString
  ST.modify $ setWhitespaceBefore (Just accStr)
  where
    consumeMoreWhitespace state = not (atEndOfInput state) && isWhitespace (curChar state)

-- | Lexes a newline token.
lexNewline :: LexerM ()
lexNewline = lift $ do
  ST.modify setTokenStartPoint
  ST.modify nextChar
  ST.modify $ addToken EndOfLineToken

-- | Lexes a file into a list of tokens.
lexFile :: Set String -> FilePath -> String -> Either String [Token]
lexFile keywords filename input =
  let -- One step in the global lexer process. Matches against the next symbol and then calls the appropriate sub-
      -- lexer.
      step :: LexerM ()
      step = do
        curChar <- lift $ ST.gets curChar
        startOfLine <- lift $ ST.gets startOfLine
        if
            | isWhitespace curChar && startOfLine -> lexIndent
            | Char.isSpace curChar -> lexWhitespace
            | Char.isDigit curChar -> lexNumber
            | Char.isAlpha curChar -> lexIdentifier keywords
            | curChar == '"' -> lexStringLiteral
            | curChar == '\n' -> lexNewline
            | otherwise -> lexSymbol

      -- Repeatedly runs the lexers until all input has been consumed.
      runner :: LexerM [Token]
      runner = do
        ei <- lift $ ST.gets atEndOfInput
        let x = traceStack (show ei) "test"
        let _ = trace x

        whileSE_ (not . atEndOfInput) step
        lift $ ST.modify $ addToken EndOfInputToken
        lift $ ST.gets tokens
   in ST.evalState (runExceptT runner) $ createLexerState input filename

{- Main -}

-- | All keywords in the language.
keywords :: Set String
keywords = Set.fromList ["Executable", "let", "print", "return"]

-- | Compiles the project in the specified file. Returns the file path to the executable that was compiled.
compile :: FilePath -> Set BuildFlag -> IO FilePath
compile fileName flags = do
  input <- readFile fileName
  let tokens = lexFile keywords fileName input
  case tokens of
    Right ts -> putStrLn $ unlines $ fmap formatBare ts
    Left err -> exitWithError $ printf "Lexer error: %s" err

  undefined

-- | Cleans up the intermediary files created while compiling the program.
cleanup :: FilePath -> Set CleanFlag -> IO ()
cleanup = undefined

main :: IO ()
main = do
  compilerName <- getProgName
  args <- getArgs

  (cmd, args_) <- assertJustWithUsageError compilerName "Command not specified." $ List.uncons args

  let cmd = head args
  let args_ = tail args
  case cmd of
    -- build Builds a project.
    "build" -> do
      (target, flagStrs) <- assertJustWithUsageError compilerName "Build target not specified." $ List.uncons args_
      flags <- getFlagsOrExit compilerName $ accumulate flagStrs
      exeFile <- compile target flags
      if isActive Run flags
        then runCmdEchoed exeFile [] True
        else pure ()

    -- clean runs cleanup.
    "clean" -> do
      (target, flagStrs) <- assertJustWithUsageError compilerName "Clea target not specified." $ List.uncons args_
      flags <- getFlagsOrExit compilerName $ accumulate flagStrs
      cleanup target flags

    -- Any other command is invalid.
    _ -> exitWithUsageError compilerName $ printf "Invalid command '%s'." cmd
  exitSuccess
