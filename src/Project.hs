module Project where

import Console (Formattable (formatBare), bold, color, format)
import qualified Data.List as List
import Lexer (Range)
import Text.Printf (printf)

{- Project -}

data Operator = Add {range :: Range} | Sub {range :: Range}

instance Formattable Operator where
  formatBare (Add range) = printf "%s %s" (formatBare range) (bold "Add")
  formatBare (Sub range) = printf "%s %s" (formatBare range) (bold "Sub")

data ProjectType = Executable {range :: Range, name :: String}

instance Formattable ProjectType where
  formatBare (Executable range name) = printf "%s %s %s" (formatBare range) (bold "Executable") (color 35 name)

{- UnheckedProject -}

data UncheckedStringLiteral = UncheckedStringLiteral {range :: Range, string :: String}

instance Formattable UncheckedStringLiteral where
  formatBare (UncheckedStringLiteral range string) =
    printf "%s %s \"%s\"" (formatBare range) (bold "UncheckedStringLiteral") (color 35 string)

data UncheckedVariable = UncheckedVariable {range :: Range, name :: String}

instance Formattable UncheckedVariable where
  formatBare (UncheckedVariable range name) =
    printf "%s %s %s" (formatBare range) (bold "UncheckedVariable") (color 35 name)

data UncheckedExpression
  = UIntLiteral {range :: Range, int :: Int}
  | UVariable {range :: Range, variable :: UncheckedVariable, param :: Maybe UncheckedExpression}
  | UBinary {range :: Range, op :: Operator, left :: UncheckedExpression, right :: UncheckedExpression}

instance Formattable UncheckedExpression where
  formatBare (UIntLiteral range int) = printf "%s %s %s" (formatBare range) (bold "UIntLiteral") (color 35 $ show int)
  formatBare (UVariable range var Nothing) = printf "%s %s\n%s" (formatBare range) (bold "UVariable") (format 1 var)
  formatBare (UVariable range var (Just param)) =
    printf "%s %s\n%s\n%s" (formatBare range) (bold "UVariable") (format 1 var) (format 1 param)
  formatBare (UBinary range op left right) =
    printf "%s %s\n%s\n%s\n%s" (formatBare range) (bold "UBinary") (format 1 op) (format 1 left) (format 1 right)

data UncheckedAssignment
  = UExprAssignment {range :: Range, expression :: UncheckedExpression}
  | UBlockAssignment {range :: Range, stmts :: [UncheckedStatement]}

instance Formattable UncheckedAssignment where
  formatBare (UExprAssignment range expr) = printf "%s %s\n%s" (formatBare range) (bold "UExprAssignment") (format 1 expr)
  formatBare (UBlockAssignment range stmts) =
    printf "%s %s\n%s" (formatBare range) (bold "UBlockAssignment") (List.intercalate "\n" $ map (format 1) stmts)

data UncheckedStatement
  = UPrintStr {range :: Range, string :: UncheckedStringLiteral}
  | UPrintExpr {range :: Range, expr :: UncheckedExpression}
  | UAssignment {range :: Range, var :: UncheckedVariable, arg :: Maybe UncheckedVariable, assmt :: UncheckedAssignment}
  | UReturn {range :: Range, expr :: UncheckedExpression}

instance Formattable UncheckedStatement where
  formatBare (UPrintStr range string) = printf "%s %s\n%s" (formatBare range) (bold "UPrintStr") (format 1 string)
  formatBare (UPrintExpr range expr) = printf "%s %s\n%s" (formatBare range) (bold "UPrintExpr") (format 1 expr)
  formatBare (UAssignment range var Nothing assmt) =
    printf "%s %s\n%s\n%s" (formatBare range) (bold "UAssignment") (format 1 var) (format 1 assmt)
  formatBare (UAssignment range var (Just arg) assmt) =
    printf "%s %s\n%s\n%s\n%s" (formatBare range) (bold "UAssignment") (format 1 var) (format 1 arg) (format 1 assmt)
  formatBare (UReturn range expr) = printf "%s %s\n%s" (formatBare range) (bold "UReturn") (format 1 expr)

data UncheckedProgram = UncheckedProgram {range :: Range, stmts :: [UncheckedStatement]}

instance Formattable UncheckedProgram where
  formatBare (UncheckedProgram range stmts) =
    printf "%s %s\n%s" (formatBare range) (bold "UncheckedProgram") (List.intercalate "\n" $ map (format 1) stmts)

data UncheckedProject = UncheckedProject {projectType :: ProjectType, program :: UncheckedProgram}

instance Formattable UncheckedProject where
  formatBare (UncheckedProject pType prog) =
    printf "%s\n%s\n%s" (bold "UncheckedProject") (format 1 pType) (format 1 prog)
