module Parser (Parser, projectParser, run) where

import Console (Formattable (formatBare))
import Control.Applicative (Alternative (empty, many, (<|>)), optional)
import Control.Monad.Loops (whileJust, whileM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State.Lazy as ST (evalState, get, gets, modify, put)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Char (isLower)
import Data.List (uncons)
import qualified Data.List as List (filter, intercalate, uncons)
import qualified Data.Maybe as Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Lexer
  ( Range,
    Token (..),
    TokenData (..),
    ignoreToken,
    isEol,
    isIndentation,
    isSeparator,
    rangeFromRanges,
    tryGetIdentifier,
    tryGetNumber,
    tryGetStringLiteral,
    tryGetSymbol,
  )
import Numeric.Natural (Natural)
import Project
import Text.Printf (printf)

{- ParserState -}

data ParserState = ParserState {input :: [Token], curTkn :: Token, prevTkn :: Token}

createParserState :: [Token] -> Either String ParserState
createParserState tokens =
  case List.uncons tokens of
    Just (cur, tail) -> Right $ ParserState {input = tail, prevTkn = cur, curTkn = cur}
    _ -> Left "Error parsing, no input."

-- | Moves to the next token in the input.
--   If the last token has been reached, this token will be moved into nxtTkn, curTKn and then prevTkn.
nextToken :: ParserState -> ParserState
nextToken state@ParserState {input, curTkn} =
  case List.uncons input of
    Just (next', input') -> state {prevTkn = curTkn, curTkn = next', input = input'}
    Nothing -> state {prevTkn = curTkn}

{- Parser types -}

data ParseResult a = Success a | Failure Text | Ignore deriving (Show)

instance Functor ParseResult where
  fmap f (Success a) = Success $ f a
  fmap _ (Failure err) = Failure err
  fmap _ Ignore = Ignore

instance Applicative ParseResult where
  pure = Success
  (<*>) (Success f) a = fmap f a
  (<*>) (Failure err1) (Failure err2) = Failure (T.intercalate "\n" [err1, err2])
  (<*>) (Failure err) _ = Failure err
  (<*>) _ (Failure err) = Failure err
  (<*>) _ _ = Ignore

instance Alternative ParseResult where
  empty = Ignore
  (<|>) (Success a) _ = Success a
  (<|>) (Failure err) _ = Failure err
  (<|>) Ignore b = b

instance Monad ParseResult where
  (>>=) (Success a) f = f a
  (>>=) (Failure err) f = Failure err
  (>>=) Ignore f = Ignore

-- | Converts a ParseResult to an Either. the Failure and Ignore cases are converted to Left.
parseResultToEither :: ParseResult a -> Either String a
parseResultToEither (Success a) = Right a
parseResultToEither (Failure err) = Left $ T.unpack err
parseResultToEither Ignore = Left "Parser produced no result."

newtype Parser s a = Parser {runParser :: s -> ParseResult (a, s)}

deriving instance Functor (Parser s)

instance Applicative (Parser s) where
  pure a = Parser $ pure . (a,)
  (<*>) (Parser f) (Parser a) = Parser $ \s ->
    case f s of
      Ignore -> Ignore
      Failure err -> Failure err
      Success (f', s') -> first f' <$> a s'

instance Alternative (Parser s) where
  empty = Parser $ const Ignore
  (<|>) (Parser a) (Parser b) = Parser $ \s -> a s <|> b s

instance Monad (Parser s) where
  (>>=) (Parser a) f = Parser $ \s ->
    case a s of
      Ignore -> Ignore
      Failure err -> Failure err
      Success (a', s') -> runParser (f a') s'

{- Default parsers -}

-- | A parser that always fails with the specified error.
pfail :: String -> Parser s a
pfail err = Parser $ const $ Failure $ T.pack err

-- | A parser that returns the current state.
get :: Parser s s
get = Parser $ \s -> pure (s, s)

-- | A parser that returns the specifed mapping over the current state.
--   Can be useful to get a specific field.
gets :: (s -> a) -> Parser s a
gets f = f <$> get

{- Application specific parsers -}

type Parser' = Parser ParserState

-- Alias for shorter for matting.
fb :: Formattable a => a -> String
fb = formatBare

-- | Runs a parser on the specified list of tokens.
run :: Parser' a -> [Token] -> Either String a
run (Parser p) input =
  fst <$> (parseResultToEither . p =<< createParserState (List.filter (not . ignoreToken . tData) input))

-- | Returns the next token from the input. Will be an EndOfInputToken every time once the end of the input has been
--   reached.
pToken :: Parser' Token
pToken = Parser $ \s ->
  let s' = nextToken s
   in pure (curTkn s, s')

-- | A parser that succeeds if the exact specified token is consumed and fails otherwise.
consumeExact :: TokenData -> String -> Parser' TokenData
consumeExact toConsume label = do
  tkn <- pToken
  if tData tkn == toConsume
    then pure $ tData tkn
    else pfail $ printf "%s Parsing %s failed, expected %s, but got %s." (fb $ tokenRange tkn) label (fb toConsume) (fb $ tData tkn)

-- | A parser that succeeds if the mapping on the consumed token returns Just, and fails otherwise.
consumeJust :: (TokenData -> Maybe a) -> String -> Parser' a
consumeJust f label = fst <$> consumeJust' f label

-- | consumeJust but also returns the token's range.
consumeJust' :: (TokenData -> Maybe a) -> String -> Parser' (a, Range)
consumeJust' f label = do
  tkn <- pToken
  case f $ tData tkn of
    Just a -> pure (a, tokenRange tkn)
    Nothing -> pfail $ printf "%s Parsing %s failed, unexpected %s." (fb $ tokenRange tkn) label (fb $ tData tkn)

-- | A parser that succeeds if the check on the consumed token returns True, and fails otherwise.
consumeIf :: (TokenData -> Bool) -> String -> Parser' ()
consumeIf f = consumeJust (\x -> if f x then Just () else Nothing)

-- | A parser that succeeds if the exact specified token is consumed and returns Ignore otherwise.
tryConsumeExact :: TokenData -> Parser' ()
tryConsumeExact toConsume = tryConsumeIf (== toConsume)

-- | A parser that succeeds if the mapping on the consumed token returns Just, and returns Ignore otherwise.
tryConsumeJust :: (TokenData -> Maybe a) -> Parser' a
tryConsumeJust f = fst <$> tryConsumeJust' f

-- | tryConsumeJust but also returns the token's range.
tryConsumeJust' :: (TokenData -> Maybe a) -> Parser' (a, Range)
tryConsumeJust' f = do
  tkn <- pToken
  case f $ tData tkn of
    Nothing -> empty
    Just a -> pure (a, tokenRange tkn)

-- | A parser that succeeds if the check on the consumed token returns True, and returns Ignore otherwise.
tryConsumeIf :: (TokenData -> Bool) -> Parser' ()
tryConsumeIf f = tryConsumeJust (\x -> if f x then Just () else Nothing)

-- | A parser that parses exactly one end of line token and fails otherwise.
eolParser :: String -> Parser' ()
eolParser label = consumeIf isEol $ printf "%a end of line" label

{- Parsers for domain types -}

-- | A parser that parses one end of line, and then any number of empty lines, optionally preceded by whitespace.
--   If the first end of line fails, the parser fails. Any other empty lines are optional.
eolsParser :: String -> Parser' ()
eolsParser label = do
  eolParser label
  () <$ many emptyLineParser
  where
    -- Parses an empty line, optionally with an indentation token in it. Returns Ignore if it was not matched.
    emptyLineParser = do
      tkn <- pToken
      case tData tkn of
        IndentationToken _ -> tryConsumeIf isEol
        EndOfLineToken _ -> pure ()
        _ -> empty

-- | A parser for a project type.
projectTypeParser :: Parser' ProjectType
projectTypeParser = do
  start <- gets (tokenRange . curTkn)
  consumeExact (KeywordToken "Executable") "project type"
  consumeExact (SymbolToken ":") "project type"
  name <- consumeJust tryGetIdentifier "project name"

  end <- gets (tokenRange . prevTkn)
  eolsParser "project type"
  pure $ Executable (rangeFromRanges start end) name

-- | A parser that consumes the specified indentation token, and returns Ignore if this was not possible.
--   An indentation of 0 means that the next token must not be an indentation token. Otherwise, a matching indentation
--   token is consumed.
indentParserM :: Natural -> Parser' ()
indentParserM 0 = do
  t <- gets (tData . curTkn)
  case t of
    IndentationToken _ -> empty
    _ -> pure ()
indentParserM indent = tryConsumeExact (IndentationToken indent)

-- | A parser  for an assignment, which can be either directly an expression, or a block of statements.
--   Will return Ignore if parsing the expression failed, or the block start was not matched (end of line).
--   Block statements will be parsed as long as the indentation is correct.
assignmentParserM :: Natural -> Parser' (UncheckedAssignment, Bool)
assignmentParserM indent = do
  startTkn <- gets curTkn
  if isEol $ tData startTkn
    then do
      eolParser "block assignment"
      start <- gets (tokenRange . curTkn)
      stmts <- many $ statementParserM (indent + 1)
      end <- gets (tokenRange . prevTkn)
      pure (UBlockAssignment (rangeFromRanges start end) stmts, False)
    else (\e -> (UExprAssignment (expressionRange e) e, True)) <$> expressionParserM

-- | A parser for an operator, returns Ignore if an operator could not be parsed.
operatorParserM :: Parser' Operator
operatorParserM = tryOp "+" Add <|> tryOp "-" Sub
  where
    tryOp sym typ = typ . snd <$> tryConsumeJust' (tryGetSymbol sym)

-- | A parser for a number, returns Ignore if a number cannot be fully parsed.
numberParserM :: Parser' (Int, Range)
numberParserM = do
  start <- gets (tokenRange . curTkn)
  isNegative <- optional (tryConsumeExact (SymbolToken "-"))
  let sign = maybe 1 (const (-1)) isNegative
  num <- tryConsumeJust tryGetNumber

  end <- gets (tokenRange . prevTkn)
  pure (sign * num, rangeFromRanges start end)

-- | A parser for an expression. Returns Ignore if any of the sub expression parsers returns ignore,
--   fails if any of the sub expression parsers fails.
expressionParserM :: Parser' UncheckedExpression
expressionParserM = binaryParserM <|> intLiteralParserM <|> variableParserM
  where
    binaryParserM :: Parser' UncheckedExpression
    binaryParserM = do
      start <- gets (tokenRange . curTkn)
      -- Left side of a binary expression cannot be a recursive expression, to prevent infinite loops.
      left <- intLiteralParserM <|> variableParserM
      op <- operatorParserM
      right <- expressionParserM

      end <- gets (tokenRange . prevTkn)
      pure $ UBinary (rangeFromRanges start end) op left right

    intLiteralParserM :: Parser' UncheckedExpression
    intLiteralParserM = uncurry (flip UIntLiteral) <$> numberParserM

    variableParserM :: Parser' UncheckedExpression
    variableParserM = do
      start <- gets (tokenRange . curTkn)
      (var, varRange) <- tryConsumeJust' tryGetIdentifier
      exprMaybe <- optional expressionParserM

      end <- gets (tokenRange . prevTkn)
      pure $ UVariable (rangeFromRanges start end) (UncheckedVariable varRange var) exprMaybe

-- | A parser for a print statement. Returns Ignore if the print keyword cannot be parsed,
--   fails if anything later fails.
printStmtParserM :: Parser' (UncheckedStatement, Bool)
printStmtParserM = do
  start <- gets (tokenRange . curTkn)
  tryConsumeExact (KeywordToken "print")

  nextToken <- gets curTkn
  strLitAndRangeMaybe <- optional $ tryConsumeJust' tryGetStringLiteral
  exprMaybe <- optional expressionParserM

  end <- gets (tokenRange . prevTkn)
  let range = rangeFromRanges start end
  case (strLitAndRangeMaybe, exprMaybe) of
    (Just (sl, r), _) -> pure (UPrintStr range (UncheckedStringLiteral r sl), True)
    (_, Just e) -> pure (UPrintExpr range e, True)
    _ ->
      pfail $
        printf
          "%s Error parsing a print statement, expected a string literal or expression, but got %s."
          (fb $ tokenRange nextToken)
          (fb nextToken)

-- | A parser for anassignment statement. Returns Ignore if the let keyword cannot be parsed,
--   fails if anything later fails.
assignmentStmtParserM :: Natural -> Parser' (UncheckedStatement, Bool)
assignmentStmtParserM indent = do
  start <- gets (tokenRange . curTkn)
  tryConsumeExact (KeywordToken "let")
  (name, nameRange) <- consumeJust' tryGetIdentifier "assignment variable"
  paramAndRangeMaybe <- optional $ tryConsumeJust' tryGetIdentifier

  consumeExact (SymbolToken "=") "assignment"
  assmtRange <- gets (tokenRange . curTkn)
  end <- gets (tokenRange . prevTkn)

  (assmt, eols) <- assignmentParserM indent <|> pfail (printf "%s: Error parsing an assignment." (fb assmtRange))

  pure
    ( UAssignment
        (rangeFromRanges start end)
        (UncheckedVariable nameRange name)
        (uncurry (flip UncheckedVariable) <$> paramAndRangeMaybe)
        assmt,
      eols
    )

-- | A parser for a return statement. Returns Ignore if the return keyword cannot be parsed,
--   fails if anything later fails.
returnStmtParserM :: Parser' (UncheckedStatement, Bool)
returnStmtParserM = do
  start <- gets (tokenRange . curTkn)
  tryConsumeExact (KeywordToken "return")
  expr <- expressionParserM <|> pfail (printf "%s Error parsing a return expression" (fb start))

  end <- gets (tokenRange . prevTkn)
  pure (UReturn (rangeFromRanges start end) expr, True)

-- | A parser for a statement. Returns Ignore if any of the sub parsers returns ignore.
--   Fails when any of the sub parsers or the  end of line parser fails.
statementParserM :: Natural -> Parser' UncheckedStatement
statementParserM indent = do
  indentParserM indent
  (stmt, eols) <- printStmtParserM <|> assignmentStmtParserM indent <|> returnStmtParserM
  if eols then do eolsParser "statement" else pure ()
  pure stmt

-- | A parser for a program.
programParser :: Parser' UncheckedProgram
programParser = do
  start <- gets (tokenRange . curTkn)
  stmts <- many (statementParserM 0)
  end <- gets (tokenRange . prevTkn)
  pure $ UncheckedProgram (rangeFromRanges start end) stmts

-- | A parser for a project.
projectParser :: Parser' UncheckedProject
projectParser = do
  projectType <- projectTypeParser

  consumeIf isSeparator "separator"
  eolsParser "separator"

  program <- programParser

  consumeExact EndOfInputToken "project"
  pure $ UncheckedProject projectType program
