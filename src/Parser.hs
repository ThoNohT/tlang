module Parser (parseProject) where

import Console (Formattable (formatBare))
import Control.Monad.Loops (whileJust, whileM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State.Lazy as ST (evalState, get, gets, modify, put)
import qualified Data.List as List (filter, uncons)
import qualified Data.Maybe as Maybe (isNothing)
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
