module Project where

import Console (Formattable (formatBare), bold, color, format)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Lexer (Range, getRange)
import Text.Printf (printf)

{- Project -}

{- An index in a list. -}
newtype Index = Index Int deriving (Show)

{- An offset in memory. -}
newtype Offset = Offset Int deriving (Show)

data StringLiteral = StringLiteral {range :: Range, index :: Index, string :: Text}

instance Formattable StringLiteral where
  formatBare uc (StringLiteral range index string) =
    printf
      "%s %s \"%s\"\nindex: {}"
      (formatBare uc range)
      (bold uc "UncheckedStringLiteral")
      (color uc 35 $ show index)
      (color uc 35 $ T.unpack string)

data Variable = Variable {range :: Range, index :: Index, offset :: Offset, name :: Text, context :: [Text]}

instance Formattable Variable where
  formatBare uc (Variable range index offset name context) =
    let ctx = if null context then "root" else printf "root::{}" (T.intercalate "::" context)
     in printf
          "%s %s %s\nindex: %s, offset: %s, context: %s, takes param: %s"
          (formatBare uc range)
          (bold uc "UncheckedVariable")
          (color uc 35 $ T.unpack name)
          (color uc 35 $ show index)
          (color uc 35 $ show offset)
          (color uc 35 ctx)

data Operator = Add {range :: Range} | Sub {range :: Range}

instance Formattable Operator where
  formatBare uc (Add range) = printf "%s %s" (formatBare uc range) (bold uc "Add")
  formatBare uc (Sub range) = printf "%s %s" (formatBare uc range) (bold uc "Sub")

data Expression
  = IntLiteral {range :: Range, value :: Int}
  | VarExpr {range :: Range, var :: Variable}
  | Binary {range :: Range, op :: Operator, left :: Expression, right :: Expression}

instance Formattable Expression where
  formatBare uc (IntLiteral range int) = printf "%s %s %s" (formatBare uc range) (bold uc "IntLiteral") (color uc 35 $ show int)
  formatBare uc (VarExpr range var) = printf "%s %s\n%s" (formatBare uc range) (bold uc "VarExpr") (format uc 1 var)
  formatBare uc (Binary range op left right) =
    printf "%s %s\n%s\n%s\n%s" (formatBare uc range) (bold uc "Binary") (format uc 1 op) (format uc 1 left) (format uc 1 right)

data Assignment
  = ExprAssignment {range :: Range, expr :: Expression}
  | BlockAssignment {range :: Range, stmts :: [Statement]}

instance Formattable Assignment where
  formatBare uc (ExprAssignment range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "ExprAssignment") (format uc 1 expr)
  formatBare uc (BlockAssignment range stmts) =
    printf "%s %s\n%s" (formatBare uc range) (bold uc "BlockAssignment") (List.intercalate "\n" $ map (format uc 1) stmts)

data Statement
  = PrintStr {range :: Range, string :: StringLiteral}
  | PrintExpr {range :: Range, expr :: Expression}
  | Assignment {range :: Range, var :: Variable, assmt :: Assignment}
  | Return {range :: Range, expr :: Expression}

instance Formattable Statement where
  formatBare uc (PrintStr range string) = printf "%s %s\n%s" (formatBare uc range) (bold uc "PrintStr") (format uc 1 string)
  formatBare uc (PrintExpr range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "PrintExpr") (format uc 1 expr)
  formatBare uc (Assignment range var  assmt) =
    printf "%s %s\n%s\n%s" (formatBare uc range) (bold uc "Assignment") (format uc 1 var) (format uc 1 assmt)
  formatBare uc (Return range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "Return") (format uc 1 expr)

data Program = Program
  { range :: Range
  , stmts :: [Statement]
  , strings :: Map Text Index
  , variablesSize :: Offset
  , variablesCount :: Index
  }

instance Formattable Program where
  formatBare uc (Program range stmts strings vs vc) =
    printf
      "%s %s\n%s\nstrings count: %s variables size: %s, variables count: %s"
      (formatBare uc range)
      (bold uc "Program")
      (List.intercalate "\n" $ map (format uc 1) stmts)
      (color uc 35 $ show $ Map.size strings)
      (color uc 35 $ show vs)
      (color uc 35 $ show vc)

data ProjectType = Executable {range :: Range, name :: Text}

instance Formattable ProjectType where
  formatBare uc (Executable range name) = printf "%s %s %s" (formatBare uc range) (bold uc "Executable") (color uc 35 (T.unpack name))

data Project = Project {projectType :: ProjectType, program :: Program}

instance Formattable Project where
  formatBare uc (Project pType program) =
    printf "%s\n%s\n%s" (bold uc "Project") (format uc 1 pType) (format uc 1 program)

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
  | UVarExpr {range :: Range, var :: UncheckedVariable}
  | UBinary {range :: Range, op :: Operator, left :: UncheckedExpression, right :: UncheckedExpression}

instance Formattable UncheckedExpression where
  formatBare uc (UIntLiteral range int) = printf "%s %s %s" (formatBare uc range) (bold uc "UIntLiteral") (color uc 35 $ show int)
  formatBare uc (UVarExpr range var) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UVarExpr") (format uc 1 var)
  formatBare uc (UBinary range op left right) =
    printf "%s %s\n%s\n%s\n%s" (formatBare uc range) (bold uc "UBinary") (format uc 1 op) (format uc 1 left) (format uc 1 right)

data UncheckedAssignment
  = UExprAssignment {range :: Range, expr :: UncheckedExpression}
  | UBlockAssignment {range :: Range, stmts :: [UncheckedStatement]}

instance Formattable UncheckedAssignment where
  formatBare uc (UExprAssignment range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UExprAssignment") (format uc 1 expr)
  formatBare uc (UBlockAssignment range stmts) =
    printf "%s %s\n%s" (formatBare uc range) (bold uc "UBlockAssignment") (List.intercalate "\n" $ map (format uc 1) stmts)

data UncheckedStatement
  = UPrintStr {range :: Range, string :: UncheckedStringLiteral}
  | UPrintExpr {range :: Range, expr :: UncheckedExpression}
  | UAssignment {range :: Range, var :: UncheckedVariable, assmt :: UncheckedAssignment}
  | UReturn {range :: Range, expr :: UncheckedExpression}

instance Formattable UncheckedStatement where
  formatBare uc (UPrintStr range string) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UPrintStr") (format uc 1 string)
  formatBare uc (UPrintExpr range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UPrintExpr") (format uc 1 expr)
  formatBare uc (UAssignment range var assmt) =
    printf "%s %s\n%s\n%s" (formatBare uc range) (bold uc "UAssignment") (format uc 1 var) (format uc 1 assmt)
  formatBare uc (UReturn range expr) = printf "%s %s\n%s" (formatBare uc range) (bold uc "UReturn") (format uc 1 expr)

isReturn :: UncheckedStatement -> Bool
isReturn (UReturn _ _) = True
isReturn _ = False

data UncheckedProgram = UncheckedProgram {range :: Range, stmts :: [UncheckedStatement]}

instance Formattable UncheckedProgram where
  formatBare uc (UncheckedProgram range stmts) =
    printf "%s %s\n%s" (formatBare uc range) (bold uc "UncheckedProgram") (List.intercalate "\n" $ map (format uc 1) stmts)

data UncheckedProject = UncheckedProject {projectType :: ProjectType, program :: UncheckedProgram}

instance Formattable UncheckedProject where
  formatBare uc (UncheckedProject pType prog) =
    printf "%s\n%s\n%s" (bold uc "UncheckedProject") (format uc 1 pType) (format uc 1 prog)
