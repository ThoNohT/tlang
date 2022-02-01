module Parser (parseProject, Parser, projectParser, run) where

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

-- TODO: Remove nextTkn once old parsers are gone.
data ParserState = ParserState {input :: [Token], curTkn :: Token, nxtTkn :: Token, prevTkn :: Token}

createParserState :: [Token] -> Either String ParserState
createParserState tokens =
  case List.uncons tokens of
    Just (cur, tail) ->
      case List.uncons tail of
        Just (next, tail') -> Right $ ParserState {input = tail', prevTkn = cur, curTkn = cur, nxtTkn = next}
        Nothing -> Right $ ParserState {input = tail, prevTkn = cur, curTkn = cur, nxtTkn = cur}
    _ -> Left "Error parsing, no input."

-- | Moves to the next token in the input.
--   If the last token has been reached, this token will be moved into nxtTkn, curTKn and then prevTkn.
nextToken :: ParserState -> ParserState
nextToken state@ParserState {input, curTkn, nxtTkn} =
  case List.uncons input of
    Just (next', input') -> state {prevTkn = curTkn, curTkn = nxtTkn, nxtTkn = next', input = input'}
    Nothing -> state {prevTkn = curTkn, curTkn = nxtTkn}

-- | Except transformer with State for ParserState and String error.
type ParserM a = ExceptT String (State ParserState) a

-- | Alternative operator for ParserM, since <|> will operate on the ExceptT transformer, and not on a Maybe inside
--   the parse result.
infixl 3 <||>

(<||>) :: ParserM (Maybe a) -> ParserM (Maybe a) -> ParserM (Maybe a)
(<||>) a b = do
  a >>= \case
    Just a' -> pure $ Just a'
    Nothing -> b

-- | Performs a second parser with the result of the first parser, only if the first parser succeeds.

-- | Performs a second parser with the result of the first parser, only if the first parser succeeds.
infixl 3 ?>>

(?>>) :: ParserM (Maybe a) -> ParserM (Maybe b) -> ParserM (Maybe (a, b))
(?>>) a b = do
  a >>= \case
    Nothing -> pure Nothing
    Just a'' ->
      b >>= \case
        Nothing -> pure Nothing
        Just b'' -> pure $ Just (a'', b'')

-- | Can be used to check that a Maybe is Just, and if it fails, raise an exception.
parserAssertJust :: String -> Maybe a -> ParserM a
parserAssertJust _ (Just a) = pure a
parserAssertJust msg Nothing = throwE msg

-- | Performs a check on the current token, and if it fails, returns an error that parsing the entity with the provided
--   label failed. If it succeeds, moves to the next token,
checkAndNext :: (Token -> Bool) -> String -> ParserM ()
checkAndNext f label = do
  t <- lift $ ST.gets curTkn
  if not $ f t
    then throwE $ printf "%s: Error parsing %s, unexpected %s." (formatBare $ tokenRange t) label (formatBare $ tData t)
    else pure ()
  lift $ ST.modify nextToken

-- | Tries to consume a token and returns it with the token's range, if the check on the token returns Just.
--   Returns Nothing otherwise. Doesn't consume a token if the check fails.
tryConsume :: (Token -> Maybe a) -> State ParserState (Maybe (a, Range))
tryConsume f = do
  t <- ST.gets curTkn
  case f t of
    Just v -> do
      ST.modify nextToken
      pure $ Just (v, tokenRange t)
    Nothing -> pure Nothing

-- | Tries to consume a token and returns it, if the check on the token returns Just. Returns Nothing otherwise.
--   Doesn't consume a token if the check fails.
tryConsume' :: (Token -> Maybe a) -> State ParserState (Maybe a)
tryConsume' f = fmap fst <$> tryConsume f

-- | Consumes the curren token and returns it with its range, if the check on the token returns Just.
--   If the check returns Nothing, returns an error that parsing the entity with the provided label failed.
consume :: (Token -> Maybe a) -> String -> ParserM (a, Range)
consume f label = do
  t <- lift $ ST.gets curTkn
  lift $ ST.modify nextToken
  case f t of
    Just v -> pure (v, tokenRange t)
    Nothing ->
      throwE $ printf "%s: Error parsing %s, unexpected %s." (formatBare $ tokenRange t) label (formatBare $ tData t)

-- | Consumes the curren token and returns, if the check on the token returns Just.
--   If the check returns Nothing, returns an error that parsing the entity with the provided label failed.
consume' :: (Token -> Maybe a) -> String -> ParserM a
consume' f label = fst <$> consume f label

-- | Consumes an end of line token.
consumeEol :: String -> ParserM ()
consumeEol label = checkAndNext (isEol . tData) $ printf "%a end of line" label

-- | Consumes one or more end of line tokens. All but the first may be prefixed by indentation tokens of any level.
consumeEols :: String -> ParserM ()
consumeEols label =
  let tryConsumeEmptyLine :: State ParserState Bool
      tryConsumeEmptyLine = do
        t <- ST.gets curTkn
        let indented = isIndentation $ tData t -- Consume any indentation first.

        -- Then check if there is an end of line token.
        t2 <- if indented then ST.gets nxtTkn else pure t
        if not $ isEol $ tData t2
          then pure False
          else do
            if indented then ST.modify nextToken else pure () -- Go ahead 2 tokens if the first was indentation.
            ST.modify nextToken
            pure True
   in do
        consumeEol label
        lift $ whileM_ tryConsumeEmptyLine (pure ())

-- | Parses a project type.
parseProjectType :: ParserM ProjectType
parseProjectType = do
  startT <- lift $ ST.gets curTkn
  checkAndNext (\t -> tData t == KeywordToken "Executable") "project type"
  checkAndNext (\t -> tData t == SymbolToken ":") "project type"
  name <- consume' (tryGetIdentifier . tData) "project name"
  endT <- lift $ ST.gets curTkn
  consumeEols "project type"
  pure $ Executable (rangeFromRanges (tokenRange startT) (tokenRange endT)) name

-- | Parses a separator between the project type definition and the program.
parseSeparator :: ParserM ()
parseSeparator = do
  checkAndNext (isSeparator . tData) "separator"
  consumeEols "separator"

-- | Checks that the next token is indented to the specified indent level.
--   For an indent level of 0, it is checked that the next token is not an indent token, and no tokens are consumed.
--   If the next token is the desired indent token (greater than 0), it is consumed.
checkIndent :: Natural -> State ParserState Bool
checkIndent indent = do
  t <- ST.gets curTkn
  case indent of
    0 -> pure $ not $ isIndentation $ tData t
    _ | tData t == IndentationToken indent -> do
      ST.modify nextToken
      pure True
    _ -> pure False

-- | Tries to parse a print statement, will return Nothing if the first keyword is not matched and fail if anything
--   later fails.
tryParsePrintStmt :: ParserM (Maybe (UncheckedStatement, Bool))
tryParsePrintStmt = do
  startToken <- lift $ ST.gets curTkn
  if tData startToken /= KeywordToken "print"
    then pure Nothing
    else do
      lift $ ST.modify nextToken

      nextToken <- lift $ ST.gets curTkn
      strLitMaybe <- lift $ fmap (\(s, r) -> UncheckedStringLiteral r s) <$> tryConsume (tryGetStringLiteral . tData)
      exprMaybe <- tryParseExpression

      endToken <- lift $ ST.gets prevTkn
      let range = rangeFromRanges (tokenRange startToken) (tokenRange endToken)

      case (strLitMaybe, exprMaybe) of
        (Just sl, _) -> pure $ Just (UPrintStr range sl, True)
        (_, Just expr) -> pure $ Just (UPrintExpr range expr, True)
        _ ->
          throwE $
            printf
              "Error parsing a print statement, expected a string literal or expression, but got %s."
              (formatBare $ tData nextToken)

-- Tries to parse a (positive or negative) number.
tryParseNumber :: State ParserState (Maybe (Int, Range))
tryParseNumber = do
  backup <- ST.get
  let isNegative = tData (curTkn backup) == SymbolToken "-"
  if isNegative then ST.modify nextToken else pure ()

  numMaybe <- fmap (\(n, r) -> if isNegative then (-1 * n, r) else (n, r)) <$> tryConsume (tryGetNumber . tData)
  if Maybe.isNothing numMaybe then ST.put backup else pure ()
  pure numMaybe

-- | Tries to parse an operator. Only consumes if an operator was parsed.
tryParseOperator :: ParserM (Maybe Operator)
tryParseOperator = tryOp "+" Add <||> tryOp "-" Sub
  where
    tryOp sym typ = lift $ fmap (\(_, r) -> typ r) <$> tryConsume (tryGetSymbol sym . tData)

-- | Tries to parse an expression. Returns None if all of the expression components return None, and fails if one of
--   the invoked sub parsers fails:
tryParseExpression :: ParserM (Maybe UncheckedExpression)
tryParseExpression = tryParseBinary <||> tryParseIntLiteral <||> tryParseVariable
  where
    restore backup = do
      lift $ ST.put backup
      pure Nothing

    -- Left side of a binary expression cannot be a recursive expression, to prevent infinite loops.
    tryParseBinary :: ParserM (Maybe UncheckedExpression)
    tryParseBinary = do
      backup <- lift ST.get
      leftOpRight <-
        (tryParseIntLiteral <||> tryParseVariable <||> restore backup)
          ?>> (tryParseOperator <||> restore backup)
          ?>> (tryParseExpression <||> restore backup)
      case leftOpRight of
        Nothing -> pure Nothing
        Just ((left, op), right) -> lift $ do
          endToken <- ST.gets prevTkn
          let range = rangeFromRanges (tokenRange $ curTkn backup) (tokenRange endToken)
          pure $ Just $ UBinary range op left right

    tryParseVariable :: ParserM (Maybe UncheckedExpression)
    tryParseVariable = do
      varMaybe <- lift $ tryConsume (tryGetIdentifier . tData)
      case varMaybe of
        Nothing -> pure Nothing
        Just (var, r) -> Just . UVariable r (UncheckedVariable r var) <$> tryParseExpression

    tryParseIntLiteral :: ParserM (Maybe UncheckedExpression)
    tryParseIntLiteral = fmap (\(n, r) -> UIntLiteral r n) <$> lift tryParseNumber

-- | Tries to parse an assignment, which can be either directly an expression, or a block of statements.
--   Will return Nothing if parsing the expressin failed, or the block start was not matched (end of line).
--   Block statements will be parsed as long as the indentation is correct.
tryParseAssignment :: Natural -> ParserM (Maybe (UncheckedAssignment, Bool))
tryParseAssignment indent = do
  startToken <- lift $ ST.gets curTkn
  if isEol $ tData startToken
    then do
      consumeEol "block assignment"
      startToken <- lift $ ST.gets curTkn

      stmts <- whileJust (tryParseStatement (indent + 1)) pure

      endToken <- lift $ ST.gets prevTkn
      pure $ Just (UBlockAssignment (rangeFromRanges (tokenRange startToken) (tokenRange endToken)) stmts, False)
    else fmap (\e -> (UExprAssignment (expressionRange e) e, True)) <$> tryParseExpression

-- | Tries to parse an assignment statement, will return Nothing if the first keyword is not matched and fail if
--   anything later fails.
tryParseAssignmentStmt :: Natural -> ParserM (Maybe (UncheckedStatement, Bool))
tryParseAssignmentStmt indent = do
  startToken <- lift $ ST.gets curTkn
  if tData startToken /= KeywordToken "let"
    then pure Nothing
    else do
      lift $ ST.modify nextToken
      (name, nameRange) <- consume (tryGetIdentifier . tData) "assignment variable"

      paramNameMaybe <- lift $ tryConsume (tryGetIdentifier . tData)

      checkAndNext ((== SymbolToken "=") . tData) "assignment"

      assmtToken <- lift $ ST.gets curTkn

      (assmt, eols) <-
        tryParseAssignment indent
          >>= parserAssertJust (printf "%s: Error parsing an assignment." (formatBare $ tokenRange assmtToken))

      endToken <- lift $ ST.gets prevTkn
      pure $
        Just
          ( UAssignment
              (rangeFromRanges (tokenRange startToken) (tokenRange endToken))
              (UncheckedVariable nameRange name)
              ((\(n, r) -> UncheckedVariable r n) <$> paramNameMaybe)
              assmt,
            eols
          )

-- | Tries to parse a return statement, will return Nothing if the first token is not a return keyword,
--   and fail if anything later fails.
tryParseReturnStmt :: ParserM (Maybe (UncheckedStatement, Bool))
tryParseReturnStmt = do
  startToken <- lift $ ST.gets curTkn
  if tData startToken /= KeywordToken "return"
    then pure Nothing
    else do
      lift $ ST.modify nextToken

      expr <-
        tryParseExpression
          >>= parserAssertJust (printf "%s: Error parsing a return expression." (formatBare $ tokenRange startToken))
      endToken <- lift $ ST.gets prevTkn
      pure $ Just (UReturn (rangeFromRanges (tokenRange startToken) (tokenRange endToken)) expr, True)

-- | Tries to parse a statement. This parser first checks whether the next token has the correct indentation, then
--   applies one of the parsers for the specific statements. Consumes no tokens if the indentation is incorrect, or no
--   statement was parsed.
tryParseStatement :: Natural -> ParserM (Maybe UncheckedStatement)
tryParseStatement indent = do
  backup <- lift ST.get
  indentIsCorrect <- lift $ checkIndent indent
  if not indentIsCorrect
    then pure Nothing
    else do
      stmtMaybe <- tryParsePrintStmt <||> tryParseAssignmentStmt indent <||> tryParseReturnStmt
      case stmtMaybe of
        Nothing -> do
          lift $ ST.put backup
          pure Nothing
        Just (stmt, eols) -> do
          if eols then consumeEols "statement" else pure ()
          pure $ Just stmt

-- | Parses a program.
parseProgram :: ParserM UncheckedProgram
parseProgram = do
  startToken <- lift $ ST.gets curTkn
  stmts <- whileJust (tryParseStatement 0) pure
  endToken <- lift $ ST.gets prevTkn
  pure $ UncheckedProgram (rangeFromRanges (tokenRange startToken) (tokenRange endToken)) stmts

-- | Parses a project from a list of tokens.
parseProject :: [Token] -> Either String UncheckedProject
parseProject input =
  ST.evalState (runExceptT runner) =<< createParserState (List.filter (not . ignoreToken . tData) input)
  where
    runner :: ParserM UncheckedProject
    runner = do
      projectType <- parseProjectType
      parseSeparator
      program <- parseProgram
      checkAndNext ((== EndOfInputToken) . tData) "project"

      pure $ UncheckedProject projectType program

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

-- | A parser that always fails with the specified error.
pfail :: String -> Parser s a
pfail err = Parser $ const $ Failure $ T.pack err

{- Default parsers -}

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

--   else fmap (\e -> (UExprAssignment (expressionRange e) e, True)) <$> tryParseExpression

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
