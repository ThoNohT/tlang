module Lexer
  ( Position (..),
    Range (..),
    Token (..),
    TokenData (..),
    rangeFromRanges,
    lexFile,
    ignoreToken,
    tryGetIdentifier,
    tryGetSymbol,
    tryGetNumber,
    tryGetStringLiteral,
    tokenToFileText,
    rangeToFileText,
    posToFileText,
    isEol,
    isSeparator,
  )
where

import Console (Formattable (formatBare), bold, color)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Lazy (State)
import qualified Control.Monad.Trans.State.Lazy as ST (evalState, get, gets, modify)
import Core (whileSE, whileSE_, whileS_)
import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Char (isDigit)
import qualified Data.Char as Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.List as List (genericLength, uncons)
import Data.Map ((!?))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set (member)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | For this lexer, we need to often match against whitespace characters that are not the newline character.
isWhitespace c = Char.isSpace c && c /= '\n'

-- | A position in a source file.
data Position = Position {line :: Int, col :: Int}

instance Formattable Position where
  formatBare Position {line, col} = printf "%i:%i" (line + 1) (col + 1)

-- | Converts a position to a string including the specified filename.
posToFileText :: FilePath -> Position -> Text
posToFileText filename Position {line, col} = T.pack $ printf "%s:%i:%i" filename line col

-- | Initial position.
zeroPos :: Position
zeroPos = Position {line = 0, col = 0}

-- | Advances the position to the next column.
nextCol :: Position -> Position
nextCol pos = pos {col = col pos + 1}

-- | Advances the position to the next line.
nextLine :: Position -> Position
nextLine Position {col, line} = Position {col = 0, line = line + 1}

-- | A range of positions in a source file.
data Range = Range {file :: FilePath, startPos :: Position, endPos :: Position}

-- | Converts a range to a string representing the range's position including the filename.
rangeToFileText :: Range -> Text
rangeToFileText Range {file, startPos} = posToFileText file startPos

instance Formattable Range where
  formatBare Range {file, startPos, endPos} =
    color 32 $ printf "[%s:%s->%s]" file (formatBare startPos) (formatBare endPos)

-- | Creates a range from two positions and a filename.
rangeFromPositions filename startPos endPos =
  Range {file = filename, startPos, endPos}

-- | Creates a range from a start and end range, using the file from the start range.
rangeFromRanges :: Range -> Range -> Range
rangeFromRanges startRange endRange =
  Range {file = file startRange, startPos = startPos startRange, endPos = endPos endRange}

-- | Encodes all the different types of tokens, with their data.
data TokenData
  = IndentationToken Natural
  | KeywordToken Text -- TODO: Replace KeywordToken with WordToken and let the parser check if a reserved word is used in the spot of an identifier?
  | IdentifierToken Text
  | SymbolToken Text
  | StringLiteralToken Text
  | NumberToken Int
  | SeparatorToken Text
  | EndOfLineToken Text
  | EndOfInputToken
  | CommentToken Text
  deriving (Eq)

-- | Indicates whether the parser should ignore this token.
ignoreToken :: TokenData -> Bool
ignoreToken (CommentToken _) = True
ignoreToken _ = False

instance Formattable TokenData where
  formatBare td = uncurry (printf "%s%s") $ bimap bold (color 35) tuple
    where
      tuple =
        case td of
          IndentationToken ind -> ("IndentationToken ", show ind)
          KeywordToken kw -> ("KeywordToken ", T.unpack kw)
          IdentifierToken id -> ("IdentifierToken ", T.unpack id)
          SymbolToken sym -> ("SymbolToken ", T.unpack sym)
          StringLiteralToken str -> ("StringLiteralToken ", show str)
          NumberToken num -> ("NumberToken ", show num)
          SeparatorToken sep -> ("SeparatorToken ", T.unpack sep)
          EndOfLineToken eol -> ("EndOfLineToken ", show eol)
          EndOfInputToken -> ("EndOfInputToken", "")
          CommentToken str -> ("CommentToken ", T.unpack str)

-- | Returns the value of an identifier in a token, if it is an identifier token, Nothing otherwise.
tryGetIdentifier :: TokenData -> Maybe Text
tryGetIdentifier (IdentifierToken id) = Just id
tryGetIdentifier _ = Nothing

-- | Returns Just if the token is a symbol token with the specified symbol, and Nothing otherwise.
tryGetSymbol :: Text -> TokenData -> Maybe Text
tryGetSymbol sym (SymbolToken sym') | sym' == sym = Just sym'
tryGetSymbol _ _ = Nothing

-- | Returns Just if the token is a symbol token with the specified symbol, and Nothing otherwise.
tryGetNumber :: TokenData -> Maybe Int
tryGetNumber (NumberToken num) = Just num
tryGetNumber _ = Nothing

-- | Returns Just if the token is a symbol token with the specified symbol, and Nothing otherwise.
tryGetStringLiteral :: TokenData -> Maybe Text
tryGetStringLiteral (StringLiteralToken str) = Just str
tryGetStringLiteral _ = Nothing

-- | Checks whether the current token is an end of line token.
isEol :: TokenData -> Bool
isEol (EndOfLineToken _) = True
isEol _ = False

-- | Checks whether the current token is a separator token.
isSeparator :: TokenData -> Bool
isSeparator (SeparatorToken _) = True
isSeparator _ = False

data Token = Token
  { tokenRange :: Range,
    tData :: TokenData,
    whitespaceBefore :: Text
  }

instance Formattable Token where
  formatBare Token {tokenRange, tData, whitespaceBefore} =
    printf "%s %s, whitespaceBefore: %s" (formatBare tokenRange) (formatBare tData) (color 35 $ show whitespaceBefore)

-- | Converts a token to a string representing the token's position including the filename.
tokenToFileText :: Token -> Text
tokenToFileText Token {tokenRange} = rangeToFileText tokenRange

data LexerState = LexerState
  { filename :: FilePath,
    input :: Text,
    -- Positions
    pos :: Position,
    prevPos :: Position,
    tokenStartPos :: Position,
    -- Output
    tokens :: [Token],
    -- Other state.
    accumulator :: Text,
    autoAccumulate :: Bool,
    curChar :: Char,
    startOfLine :: Bool,
    spacesPerIndent :: Maybe Natural,
    whitespaceBefore :: Text,
    atEndOfInput :: Bool
  }

-- | Creates a lexer state from the specified input and filename.
createLexerState :: Text -> FilePath -> LexerState
createLexerState input filename =
  LexerState
    { filename,
      input = sanitizedInput,
      pos = zeroPos,
      prevPos = zeroPos,
      tokenStartPos = zeroPos,
      tokens = [],
      accumulator = "",
      autoAccumulate = True,
      curChar = firstChar,
      startOfLine = True,
      spacesPerIndent = Nothing,
      whitespaceBefore = "",
      atEndOfInput
    }
  where
    (atEndOfInput, (firstChar, sanitizedInput)) =
      maybe (True, ('\0', "")) (False,) $ T.uncons input

-- | Moves to the next character in the lexer state.
--   If autoAccumulate is enabled then the tokens will be added to accumulator, until the end of the input is reached.
--   then the current char will be null, but no characters are accumulated.
nextChar :: LexerState -> LexerState
nextChar state =
  let -- Updates the position to the next column or line depending on the current character.
      updatePosition state = case curChar state of
        '\n' -> state {pos = nextLine $ pos state, prevPos = pos state, startOfLine = True}
        _ -> state {pos = nextCol $ pos state, prevPos = pos state, startOfLine = False}

      -- Sets curChar to the next character.
      setNextChar :: LexerState -> LexerState
      setNextChar state = case T.uncons $ input state of
        Just (nextChar, input') ->
          let newState = state {curChar = nextChar, input = input'}
           in if autoAccumulate state then accumulateChar (curChar state) newState else newState
        Nothing -> state {atEndOfInput = True, curChar = '\0'}
   in if atEndOfInput state
        then state
        else setNextChar $ updatePosition state

-- | Sets the token start position to the current position.
--   This position will be the start position for the next token added to the output.
setTokenStartPoint :: LexerState -> LexerState
setTokenStartPoint state = state {tokenStartPos = pos state, accumulator = ""}

-- | Sets the whitespaceBefore field to the specified value.
setWhitespaceBefore :: Text -> LexerState -> LexerState
setWhitespaceBefore value state = state {whitespaceBefore = value}

-- | Adds a token to the output list.
--   Uses the state's backup position as the start of the token, and the previous position as the end.
--   whitespaceBefore is reset to the empty string.
addToken :: TokenData -> LexerState -> LexerState
addToken tData state@LexerState {tokens, whitespaceBefore, filename, tokenStartPos, prevPos} =
  state {tokens = newToken : tokens, whitespaceBefore = ""}
  where
    newToken =
      Token
        { tokenRange = rangeFromPositions filename tokenStartPos prevPos,
          whitespaceBefore = whitespaceBefore,
          tData = tData
        }

-- | Returns the accumulated string since the last start point.
accumulatedText :: LexerState -> Text
accumulatedText LexerState {accumulator} = T.reverse accumulator

-- | Sets autoAccumulate to the specified value.
setAutoAccumulate :: Bool -> LexerState -> LexerState
setAutoAccumulate val state = state {autoAccumulate = val}

-- | Adds a character to the accumulator.
accumulateChar :: Char -> LexerState -> LexerState
accumulateChar val state@LexerState {accumulator} = state {accumulator = T.cons val accumulator}

-- | Except transformer with State for LexerState and Text error.
type LexerM a = ExceptT Text (State LexerState) a

-- | Can be used to check a predicate based on the current state, and if it fails, raise an exception,
--   including some location data.
checkLexerPredicate :: (LexerState -> Bool) -> Text -> LexerM ()
checkLexerPredicate pred msg = do
  s <- lift ST.get
  if pred s
    then pure ()
    else throwE $ T.pack $ printf "%s: Lexer error: %s" (posToFileText (filename s) (pos s)) msg

-- | Can be used to check a predicate, and if it fails, raise an exception, including some location data.
checkLexerPredicate' :: Bool -> Text -> LexerM ()
checkLexerPredicate' True msg = pure ()
checkLexerPredicate' False msg = do
  s <- lift ST.get
  throwE $ T.pack $ printf "%s: Lexer error: %s" (posToFileText (filename s) (pos s)) msg

-- | Can be used to check that a Maybe is Just, and if it fails, raise an exception, including some location data.
lexerAssertJust :: Maybe a -> Text -> LexerM a
lexerAssertJust (Just val) msg = pure val
lexerAssertJust Nothing msg = do
  s <- lift ST.get
  throwE $ T.pack $ printf "%s: Lexer error: %s" (posToFileText (filename s) (pos s)) msg

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
          ( do
              checkLexerPredicate ((==) ' ' . curChar) "Leading whitespace may only consist of whitespaces."
              lift $ ST.modify nextChar
          )
      let nSpaces = List.genericLength spaces
      let sOffset = nSpaces `rem` spi'
      let prefix = T.pack "Invalid number of leading spaces. Expected a multiple of"
      checkLexerPredicate' (sOffset == 0) $
        T.pack $ printf "%s %i, but got %i, which %i too many or %i too few." prefix spi' nSpaces sOffset (spi' - sOffset)
      lift $ ST.modify $ addToken $ IndentationToken (nSpaces `div` spi')

-- | Lexes a number, simply a token with a value as long as the characters are numeric.
lexNumber :: LexerM ()
lexNumber = do
  lift $ ST.modify setTokenStartPoint
  lift $ whileS_ (\s -> not (atEndOfInput s) && isDigit (curChar s)) (ST.modify nextChar)
  nrStr <- lift $ ST.gets accumulatedText
  let nrMaybe = readMaybe (T.unpack nrStr) :: Maybe Int
  nr <- lexerAssertJust nrMaybe "Failed to parse a number."
  lift $ ST.modify $ addToken $ NumberToken nr

-- | Lexes an identifier or a keyword, consumes characters as long as they are alphanumeric. If the resulting name is
--   contained in the set of keywords, a keyword token is added, otherwise an identifier token is added.
lexIdentifier :: Set Text -> LexerM ()
lexIdentifier keywords = lift $ do
  ST.modify setTokenStartPoint
  whileS_ (\s -> not (atEndOfInput s) && Char.isAlphaNum (curChar s)) (ST.modify nextChar)
  id <- ST.gets accumulatedText
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
            lift $ forM_ (T.unpack escapedStr) (ST.modify . accumulateChar)
          else lift $ ST.modify $ accumulateChar curChar

        lift $ ST.modify nextChar
    )

  assertNotAtEnd ""
  lift $ ST.modify nextChar
  str <- lift $ ST.gets accumulatedText
  lift $ ST.modify $ addToken $ StringLiteralToken str

  lift $ ST.modify $ setAutoAccumulate True
  where
    mappedChars :: Map Char Char
    mappedChars =
      Map.fromList [('\\', '\\'), ('/', '/'), ('b', '\x08'), ('f', '\x0c'), ('n', '\n'), ('r', '\r'), ('t', '\t')]

    lexEscapedChar :: LexerM Text
    lexEscapedChar = do
      lift $ ST.modify nextChar
      assertNotAtEnd "escaped character in "
      curChar <- lift $ ST.gets curChar
      pure $ case mappedChars !? curChar of
        Just mc -> T.singleton mc
        Nothing -> T.pack ['\\', curChar]

    assertNotAtEnd subject =
      checkLexerPredicate
        (not . atEndOfInput)
        $ T.pack $ printf "Input ended before %sstring literal ended." (T.unpack subject)

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
      sep <- ST.gets accumulatedText
      ST.modify $ addToken $ SeparatorToken sep
    ('-', _) -> lexRegularSymbol
    ('/', '/') -> do
      -- Two / indicate a single-line comment. Just collect until a newline is consumed.
      whileS_ (\s -> not (atEndOfInput s) && curChar s /= '\n') (ST.modify nextChar)
      comment <- ST.gets accumulatedText
      ST.modify $ addToken $ CommentToken comment
      -- Move on to the next line.
      ST.modify nextChar
    _ -> lexRegularSymbol
  where
    isSymbolChar c = not $ Char.isSpace c || Char.isAlphaNum c

    lexRegularSymbol :: State LexerState ()
    lexRegularSymbol = do
      whileS_ (\s -> not (atEndOfInput s) && isSymbolChar (curChar s)) (ST.modify nextChar)
      sym <- ST.gets accumulatedText
      if not $ T.null sym then ST.modify $ addToken $ SymbolToken sym else pure ()

-- | Lexes some whitespace, sets whitespaceBefore to the whitespace that was parsed.
lexWhitespace :: LexerM ()
lexWhitespace = lift $ do
  ST.modify setTokenStartPoint
  whileS_ consumeMoreWhitespace (ST.modify nextChar)
  accStr <- ST.gets accumulatedText
  ST.modify $ setWhitespaceBefore accStr
  where
    consumeMoreWhitespace state = not (atEndOfInput state) && isWhitespace (curChar state)

-- | Lexes a newline token.
lexNewline :: LexerM ()
lexNewline = lift $ do
  ST.modify setTokenStartPoint
  prevChar <- ST.gets curChar
  ST.modify nextChar
  curChar <- ST.gets curChar
  if prevChar == '\r' && curChar == '\n' then ST.modify nextChar else pure ()
  newlineStr <- ST.gets accumulatedText
  ST.modify $ addToken $ EndOfLineToken newlineStr

-- | Lexes a file into a list of tokens.
lexFile :: Set Text -> FilePath -> Text -> Either Text [Token]
lexFile keywords filename input = second reverse $ ST.evalState (runExceptT runner) $ createLexerState input filename
  where
    -- One step in the global lexer process. Matches against the next symbol and then calls the appropriate sub lexer.
    step :: LexerM ()
    step = do
      curChar <- lift $ ST.gets curChar
      startOfLine <- lift $ ST.gets startOfLine
      if
          | isWhitespace curChar && startOfLine -> lexIndent
          | isWhitespace curChar -> lexWhitespace
          | Char.isDigit curChar -> lexNumber
          | Char.isAlpha curChar -> lexIdentifier keywords
          | curChar == '"' -> lexStringLiteral
          | curChar == '\n' || curChar == '\r' -> lexNewline
          | otherwise -> lexSymbol

    -- Repeatedly runs the lexers until all input has been consumed.
    runner :: LexerM [Token]
    runner = do
      whileSE_ (not . atEndOfInput) step
      lift $ ST.modify $ addToken EndOfInputToken
      lift $ ST.gets tokens
