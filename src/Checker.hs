module Checker (checkProject) where

import Console (Formattable (formatBare), color)
import Data.Foldable (foldl)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE (fromList, prependList, toList)
import Data.Map (Map)
import Data.Map qualified as Map (empty)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lexer (Range)
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
type Context = [String]

data CheckerContext = CheckerContext
  { -- The strings known throughout the program and their indexes.
    stringIndexes :: Map String Index,
    -- The variables known in the current scope and their index an offsets.
    variableOffsets :: Map String (Index, Offset),
    -- The index for the next variable to be defined.
    nextVariableIndex :: Index,
    -- The offset for the next variable to be defined.
    nextVariableOffset :: Offset
  }

checkStatement :: Context -> (CheckerContext, [CheckResult Statement]) -> UncheckedStatement -> (CheckerContext, [CheckResult Statement])
checkStatement = undefined

{- Checks a program for issues. -}
checkProgram :: UncheckedProgram -> CheckResult Program
checkProgram (UncheckedProgram r stmts) =
  let initialCtx =
        CheckerContext
          { stringIndexes = Map.empty,
            variableOffsets = Map.empty,
            nextVariableIndex = Index 0,
            nextVariableOffset = Offset 0
          }
      (ctx', checkedStmts) = foldl (checkStatement []) (initialCtx, []) stmts
      stmtIssues = concatMap checkResultIssues checkedStmts
   in -- TODO: The last statement must be a return statement.
      -- TODO: The first statement after a return is not reachable.
      if not $ any ((==) Error . severity) stmtIssues
        then
          Checked
            stmtIssues
            ( Program
                { range = r,
                  stmts = mapMaybe checkResultValue checkedStmts,
                  strings = stringIndexes ctx',
                  variablesCount = nextVariableIndex ctx',
                  variablesSize = nextVariableOffset ctx'
                }
            )
        else Failed $ NE.fromList stmtIssues

{- Checks a project for issues. -}
checkProject :: UncheckedProject -> CheckResult Project
checkProject (UncheckedProject projectType program) = Project projectType <$> checkProgram program
