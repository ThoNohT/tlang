module Project where

import Console (Formattable (formatBare), bold, color, format)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Lexer (Range)
import Text.Printf (printf)

{- Project -}

data Operator = Add {range :: Range} | Sub {range :: Range}

instance Formattable Operator where
  formatBare uc (Add range) = printf "%s %s" (formatBare uc range) (bold uc "Add")
  formatBare uc (Sub range) = printf "%s %s" (formatBare uc range) (bold uc "Sub")

data ProjectType = Executable {range :: Range, name :: Text}

instance Formattable ProjectType where
  formatBare uc (Executable range name) = printf "%s %s %s" (formatBare uc range) (bold uc "Executable") (color uc 35 (T.unpack name))

{- UnheckedProject -}

data UncheckedStringLiteral = UncheckedStringLiteral {range :: Range, string :: Text}

instance Formattable UncheckedStringLiteral where
  formatBare uc (UncheckedStringLiteral range string) =
    printf "%s %s \"%s\"" (formatBare uc range) (bold uc "UncheckedStringLiteral") (color uc 35 $ T.unpack string)

data UncheckedVariable = UncheckedVariable {range :: Range, name :: Text}

instance Formattable UncheckedVariable where
  formatBare uc (UncheckedVariable range name) =
    printf "%s %s %s" (formatBare uc range) (bold uc "UncheckedVariable") (color uc 35 $ T.unpack name)

data UncheckedExpression
  = UIntLiteral {range :: Range, int :: Int}
  | UVariable {range :: Range, variable :: UncheckedVariable, param :: Maybe UncheckedExpression}
  | UBinary {range :: Range, op :: Operator, left :: UncheckedExpression, right :: UncheckedExpression}

-- | Extracts the range from an expression.
expressionRange :: UncheckedExpression -> Range
expressionRange UIntLiteral {range} = range
expressionRange UVariable {range} = range
expressionRange UBinary {range} = range

instance Formattable UncheckedExpression where
  formatBare uc (UIntLiteral range int) = printf "%s %s %s" (formatBare uc range) (bold uc "UIntLiteral") (color uc 35 $ show int)
  formatBare uc (UVariable range var Nothing) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UVariable") (format uc 1 var)
  formatBare uc (UVariable range var (Just param)) =
    printf "%s %s\n%s\n%s" (formatBare uc range) (bold uc "UVariable") (format uc 1 var) (format uc 1 param)
  formatBare uc (UBinary range op left right) =
    printf "%s %s\n%s\n%s\n%s" (formatBare uc range) (bold uc "UBinary") (format uc 1 op) (format uc 1 left) (format uc 1 right)

data UncheckedAssignment
  = UExprAssignment {range :: Range, expression :: UncheckedExpression}
  | UBlockAssignment {range :: Range, stmts :: [UncheckedStatement]}

instance Formattable UncheckedAssignment where
  formatBare uc (UExprAssignment range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UExprAssignment") (format uc 1 expr)
  formatBare uc (UBlockAssignment range stmts) =
    printf "%s %s\n%s" (formatBare uc range) (bold uc "UBlockAssignment") (List.intercalate "\n" $ map (format uc 1) stmts)

data UncheckedStatement
  = UPrintStr {range :: Range, string :: UncheckedStringLiteral}
  | UPrintExpr {range :: Range, expr :: UncheckedExpression}
  | UAssignment {range :: Range, var :: UncheckedVariable, arg :: Maybe UncheckedVariable, assmt :: UncheckedAssignment}
  | UReturn {range :: Range, expr :: UncheckedExpression}

instance Formattable UncheckedStatement where
  formatBare uc (UPrintStr range string) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UPrintStr") (format uc 1 string)
  formatBare uc (UPrintExpr range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UPrintExpr") (format uc 1 expr)
  formatBare uc (UAssignment range var Nothing assmt) =
    printf "%s %s\n%s\n%s" (formatBare uc range) (bold uc "UAssignment") (format uc 1 var) (format uc 1 assmt)
  formatBare uc (UAssignment range var (Just arg) assmt) =
    printf "%s %s\n%s\n%s\n%s" (formatBare uc range) (bold uc "UAssignment") (format uc 1 var) (format uc 1 arg) (format uc 1 assmt)
  formatBare uc (UReturn range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UReturn") (format uc 1 expr)

data UncheckedProgram = UncheckedProgram {range :: Range, stmts :: [UncheckedStatement]}

instance Formattable UncheckedProgram where
  formatBare uc (UncheckedProgram range stmts) =
    printf "%s %s\n%s" (formatBare uc range) (bold uc "UncheckedProgram") (List.intercalate "\n" $ map (format uc 1) stmts)

data UncheckedProject = UncheckedProject {projectType :: ProjectType, program :: UncheckedProgram}

instance Formattable UncheckedProject where
  formatBare uc (UncheckedProject pType prog) =
    printf "%s\n%s\n%s" (bold uc "UncheckedProject") (format uc 1 pType) (format uc 1 prog)
