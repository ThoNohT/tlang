module Checker (checkProject) where

import Console (Formattable (formatBare), color)
import Control.Monad (foldM)
import Control.Monad.Trans.State (State, evalState, get, gets, modify')
import Core (tryHead, tryLast)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (foldl)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE (fromList, head, prependList, tail, toList)
import Data.Map (Map, (!?))
import qualified Data.Map as Map (empty, insert, size)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lexer (Range, WithRange (getRange), rangeFromRanges)
import Project
import Text.Printf (printf)

{- The different severities for a check issue. An Error prevents compilation. -}
data CheckSeverity = Error | Warning deriving (Eq)

instance Formattable CheckSeverity where
  formatBare uc Error = color uc 31 "Error"
  formatBare uc Warning = color uc 33 "Warning"

{- An issue found when checking a project. -}
data CheckIssue = CheckIssue {range :: Range, severity :: CheckSeverity, msg :: Text}

instance Formattable CheckIssue where
  formatBare uc CheckIssue {range, severity, msg} =
    printf "%s: %s: %s" (formatBare uc range) (formatBare uc severity) msg

{- The result of a check. Even a successful check result can have issues. -}
data CheckResult a = Checked [CheckIssue] a | Failed (NonEmpty CheckIssue)

{- Returns the value from a check result, or Nothing if it was failed. -}
checkResultValue :: CheckResult a -> Maybe a
checkResultValue (Checked _ r) = Just r
checkResultValue _ = Nothing

{- Returns the issues in a check result. -}
checkResultIssues :: CheckResult a -> [CheckIssue]
checkResultIssues (Checked issues _) = issues
checkResultIssues (Failed issues) = NE.toList issues

instance WithRange a => WithRange (CheckResult a) where
  getRange (Checked _ a) = getRange a
  -- TODO: not sure if the ranges are ordered, rangeFromRanges does assume this.
  getRange (Failed issues) = foldl rangeFromRanges (getRange $ NE.head issues) (getRange <$> NE.tail issues)

instance Functor CheckResult where
  fmap f (Checked issues r) = Checked issues $ f r
  fmap f (Failed issues) = Failed issues

instance Applicative CheckResult where
  pure = Checked []
  af <*> aa = case af of
    Failed issues -> Failed issues
    Checked issues f -> case aa of
      Failed issues' -> Failed $ NE.prependList issues issues'
      Checked issues' a -> Checked (issues <> issues') (f a)

instance Monad CheckResult where
  ma >>= mf = case ma of
    Failed issues -> Failed issues
    Checked issues a -> case mf a of
      Failed issues' -> Failed $ NE.prependList issues issues'
      Checked issues' a' -> Checked (issues <> issues') a'

{- Represents the nested set of variables the current statement is being evaluated in. Used for displaying
   friendly debug information. -}
type Context = [Text]

data CheckerContext = CheckerContext
  { -- The strings known throughout the program and their indexes.
    stringIndexes :: Map Text Index
  , -- The variables known in the current scope and their index an offsets.
    variableOffsets :: Map Text (Index, Offset)
  , -- The index for the next variable to be defined.
    nextVariableIndex :: Index
  , -- The offset for the next variable to be defined.
    nextVariableOffset :: Offset
  , -- The current context.
    context :: Context
  }

{- The monad in which the checker runs, contains state, and returns a CheckResult. -}
type CheckerM a = CheckerM' (CheckResult a)

{- CheckerM, but not with a CheckResult. -}
type CheckerM' a = State CheckerContext a

{- Returns the string index for the specified string and updates the set of string literals. If the string was defined
   before, this index is returned to prevent allocating a new string. -}
getStringIndex :: Text -> CheckerM' Index
getStringIndex str = do
  stringIndexes' <- gets stringIndexes
  case stringIndexes' !? str of
    Just idx -> pure idx
    Nothing -> do
      let idx = Index $ Map.size $ stringIndexes'
      modify' (\s -> s {stringIndexes = Map.insert str idx stringIndexes'})
      pure idx

defineVariable :: Range -> Context -> CheckerContext -> Text -> Bool -> Bool -> (CheckerContext, CheckResult Variable)
defineVariable range context checkCtxt name takesParam isArg = undefined

checkExpr :: CheckerContext -> UncheckedExpression -> (CheckerContext, CheckResult Expression)
checkExpr = undefined

-- checkStatement ::
--   Context ->
--   (CheckerContext, [CheckResult Statement]) ->
--   UncheckedStatement ->
--   (CheckerContext, [CheckResult Statement])
-- checkStatement context (checkCtx, acc) = \case
--   UPrintStr r (UncheckedStringLiteral r2 str) ->
--     let (checkCtx', idx) = getStringIndex checkCtx str
--      in (checkCtx', pure (PrintStr r $ StringLiteral r2 idx str) : acc)
--   UPrintExpr r expr ->
--     let (checkCtx', expr') = checkExpr checkCtx expr
--      in (checkCtx, (PrintExpr r <$> expr') : acc)
--   UAssignment r (UncheckedVariable r2 varName) maybeParam expr ->
--     let innerCtx = varName : context
--         paramResultMaybe =
--           ( \(UncheckedVariable r3 pName) ->
--               defineVariable r3 innerCtx checkCtx pName False True
--           )
--             <$> maybeParam
--      in undefined
--   _ -> undefined

checkStatement :: UncheckedStatement -> CheckerM Statement
checkStatement = undefined

{- Given a function that performs a calculation on an element in a monad, creates a function that can be used in
   foldM that takes an accumulator list of result elements and a new source element and prepends the result of
   processing the source element to the accumulator. -}
inFold :: Monad m => (a -> m b) -> [b] -> a -> m [b]
inFold f' acc elem = (: acc) <$> f' elem

{- Checks a program for issues. -}
checkProgram :: UncheckedProgram -> CheckerM Program
checkProgram (UncheckedProgram r stmts) = do
  checkedStmts <- reverse <$> foldM (inFold checkStatement) [] stmts
  let stmtIssues = concatMap checkResultIssues checkedStmts

  -- The last statement must be a return statement.
  let lastStmtIssues = case tryLast stmts of
        Nothing -> [CheckIssue {range = r, severity = Error, msg = "A program needs at least one statement."}]
        Just (UReturn _ _) -> []
        Just _ -> [CheckIssue {range = r, severity = Error, msg = "The last statement of a program needs to be a return."}]

  -- The first statement after a return statement is unreachable.
  let unreachableIssues = case stmts & dropWhile (not . isReturn) & drop 1 & tryHead of
        Nothing -> []
        Just stmt -> [CheckIssue {range = getRange stmt, severity = Warning, msg = "Statement is unreachable"}]

  let allIssues = concat [stmtIssues, lastStmtIssues, unreachableIssues]
  pure undefined

  if not $ any ((==) Error . severity) stmtIssues
    then do
      ctx <- get
      pure $
        Checked
          stmtIssues
          ( Program
              { range = r
              , stmts = mapMaybe checkResultValue checkedStmts
              , strings = stringIndexes ctx
              , variablesCount = nextVariableIndex ctx
              , variablesSize = nextVariableOffset ctx
              }
          )
    else pure $ Failed $ NE.fromList stmtIssues

{- Checks a project for issues. -}
checkProject :: UncheckedProject -> CheckResult Project
checkProject (UncheckedProject projectType program) =
  Project projectType <$> evalState (checkProgram program) initialContext
 where
  initialContext =
    CheckerContext
      { stringIndexes = Map.empty
      , variableOffsets = Map.empty
      , nextVariableIndex = Index 0
      , nextVariableOffset = Offset 0
      , context = []
      }
