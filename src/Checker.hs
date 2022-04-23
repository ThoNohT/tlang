module Checker (checkProject) where

import Console (Formattable (formatBare), color)
import Control.Monad (foldM)
import Control.Monad.Trans.State (State, evalState, get, gets, modify')
import Core (tryHead, tryLast)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (foldl)
import Data.Function ((&))
import Data.Functor ((<&>))
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

data CheckerContext = CheckerContext
  { -- The strings known throughout the program and their indexes.
    stringIndexes :: Map Text Index
  , -- The variables known in the current scope and their index an offsets.
    variableOffsets :: Map Text (Index, Offset)
  , -- The index for the next variable to be defined.
    nextVariableIndex :: Index
  , -- The offset for the next variable to be defined.
    nextVariableOffset :: Offset
  , -- The current nesting context. Represents the nested set of variables the current statement is being evaluated in.
    -- used for displaying friendly debug information.
    nestingCtx :: [Text]
  }

{- The monad in which the checker runs, contains state, and returns a CheckResult. -}
type CheckerM a = CheckerM' (CheckResult a)

{- CheckerM, but not with a CheckResult. -}
type CheckerM' a = State CheckerContext a

{- Pushes a new variable on the nesting context. -}
pushNestingCtx :: Text -> CheckerM' ()
pushNestingCtx name = modify' (\s -> s {nestingCtx = name : (nestingCtx s)})

{- Pops the last variable from the nesting contex. -}
popNestingCtx :: CheckerM' ()
popNestingCtx = modify' (\s -> s {nestingCtx = drop 1 (nestingCtx s)})

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

defineVariable :: Range -> Text -> CheckerM Variable
defineVariable range name = undefined

getVariable :: Range -> Text -> CheckerM Variable
getVariable range name = undefined

checkExpr :: UncheckedExpression -> CheckerM Expression
checkExpr (UIntLiteral r intVal) = pure $ Checked [] (IntLiteral r intVal)
checkExpr (UVarExpr r1 (UncheckedVariable r2 name)) = fmap (VarExpr r1) <$> getVariable r2 name
checkExpr (UBinary r op exL exR) = do
  cExL <- checkExpr exL
  cExR <- checkExpr exR
  pure $ cExL >>= \le -> cExR <&> \re -> Binary r op le re

checkAssignment :: UncheckedAssignment -> CheckerM Assignment
checkAssignment (UExprAssignment r expr) = fmap (ExprAssignment r) <$> checkExpr expr
checkAssignment (UBlockAssignment r stmts) = do
  -- Store variables state such that after this block is done, we can revert assignments of all local variables.
  variablesTop <- gets variableOffsets
  checkedStmts <- mapM checkStatement stmts
  let stmtIssues = concatMap checkResultIssues checkedStmts

  -- Revert variables, but not next variable index, such that all still get a unique index.
  modify' (\s -> s {variableOffsets = variablesTop})

  -- TODO: When types are introduced, all returns need to be of the same type.
  let lastStmtIssues = case tryLast stmts of
        -- The last statement must be a return statement.
        Nothing -> [CheckIssue {range = r, severity = Error, msg = "A program needs at least one statement."}]
        Just (UReturn _ _) -> []
        Just _ -> [CheckIssue {range = r, severity = Error, msg = "The last statement of a program needs to be a return."}]

  -- The first statement after a return statement is unreachable.
  let unreachableIssues = case stmts & dropWhile (not . isReturn) & drop 1 & tryHead of
        Nothing -> []
        Just stmt -> [CheckIssue {range = getRange stmt, severity = Warning, msg = "Statement is unreachable"}]

  let allIssues = concat [stmtIssues, lastStmtIssues, unreachableIssues]

  if not $ any ((==) Error . severity) allIssues
    then pure $ Checked allIssues (BlockAssignment r (mapMaybe checkResultValue checkedStmts))
    else pure $ Failed $ NE.fromList allIssues

checkStatement :: UncheckedStatement -> CheckerM Statement
checkStatement (UPrintStr r1 (UncheckedStringLiteral r2 str)) = do
  idx <- getStringIndex str
  pure $ Checked [] $ PrintStr r1 (StringLiteral r2 idx str)
checkStatement (UPrintExpr r1 expr) = fmap (PrintExpr r1) <$> checkExpr expr
checkStatement (UAssignment r1 (UncheckedVariable r2 name) assmt) = do
  -- Check the assignment before the variable so the variable is not yet known during assignment evaluation.
  -- But do check thevariable even if the assignment fails, so it is known later.
  pushNestingCtx name
  assignment <- checkAssignment assmt
  popNestingCtx

  variable <- defineVariable r2 name

  pure $ assignment >>= (\assmt -> variable <&> (\var -> Assignment r1 var assmt))
checkStatement (UReturn r expr) = fmap (Return r) <$> checkExpr expr

{- Checks a program for issues. -}
checkProgram :: UncheckedProgram -> CheckerM Program
checkProgram (UncheckedProgram r stmts) = do
  checkedStmts <- reverse <$> mapM checkStatement stmts
  let stmtIssues = concatMap checkResultIssues checkedStmts

  let lastStmtIssues = case tryLast stmts of
        -- The last statement must be a return statement.
        Nothing -> [CheckIssue {range = r, severity = Error, msg = "A program needs at least one statement."}]
        Just (UReturn _ _) -> []
        Just _ -> [CheckIssue {range = r, severity = Error, msg = "The last statement of a program needs to be a return."}]

  -- The first statement after a return statement is unreachable.
  let unreachableIssues = case stmts & dropWhile (not . isReturn) & drop 1 & tryHead of
        Nothing -> []
        Just stmt -> [CheckIssue {range = getRange stmt, severity = Warning, msg = "Statement is unreachable"}]

  let allIssues = concat [stmtIssues, lastStmtIssues, unreachableIssues]

  if not $ any ((==) Error . severity) allIssues
    then do
      ctx <- get
      pure $
        Checked
          allIssues
          ( Program
              { range = r
              , stmts = mapMaybe checkResultValue checkedStmts
              , strings = stringIndexes ctx
              , variablesCount = nextVariableIndex ctx
              , variablesSize = nextVariableOffset ctx
              }
          )
    else pure $ Failed $ NE.fromList allIssues

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
      , nestingCtx = []
      }
