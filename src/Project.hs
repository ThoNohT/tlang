module Project where

import Console (Formattable (formatBare), bold, color, format)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Lexer (Range, getRange)
import StrFmt

{- Assignment extraction -}
class GetAssignments a where
  getAssignments :: a -> [(Variable, Assignment)]

{- Project -}

-- | An index in a list.
newtype Index = Index Int deriving (Show, Num)

-- | An offset in memory.
newtype Offset = Offset Int deriving (Show, Num)

-- | A string literal, including its index among all string literals.
data StringLiteral = StringLiteral {range :: Range, index :: Index, string :: Text}

instance Formattable StringLiteral where
  formatBare uc (StringLiteral range index string) =
    sfmt
      (str % " " % str % " \"" % str % "\"\nindex: " % str)
      (formatBare uc range)
      (bold uc "StringLiteral")
      (color uc 35 $ T.unpack string)
      (color uc 35 $ show index)

-- | A variable, including its index among all variables, its offset in memory, and the context in which it is defined.
data Variable = Variable {range :: Range, index :: Index, offset :: Offset, name :: Text, context :: [Text]}

instance Formattable Variable where
  formatBare uc (Variable range index offset name context) =
    let ctx = if null context then "root" else sfmt ("root::" % txt) (T.intercalate "::" context)
     in sfmt
          (str % " " % str % " " % str % "\nindex: " % str % ", offset: " % str % ", context: " % str)
          (formatBare uc range)
          (bold uc "Variable")
          (color uc 35 $ T.unpack name)
          (color uc 35 $ show index)
          (color uc 35 $ show offset)
          (color uc 35 ctx)

-- | An operator.
data Operator = Add {range :: Range} | Sub {range :: Range}

instance Formattable Operator where
  formatBare uc (Add range) = sfmt (str % " " % str) (formatBare uc range) (bold uc "Add")
  formatBare uc (Sub range) = sfmt (str % " " % str) (formatBare uc range) (bold uc "Sub")

-- | An expression used for calculating a value.
data Expression
  = IntLiteral {range :: Range, value :: Int}
  | VarExpr {range :: Range, var :: Variable}
  | Binary {range :: Range, op :: Operator, left :: Expression, right :: Expression}

instance Formattable Expression where
  formatBare uc (IntLiteral range int) = sfmt (str % " " % str % " " % str) (formatBare uc range) (bold uc "IntLiteral") (color uc 35 $ show int)
  formatBare uc (VarExpr range var) = sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "VarExpr") (format uc 1 var)
  formatBare uc (Binary range op left right) =
    sfmt (str % " " % str % "\n" % str % "\n" % str % "\n" % str) (formatBare uc range) (bold uc "Binary") (format uc 1 op) (format uc 1 left) (format uc 1 right)

-- | The assignment of an expression, or a block of statements ending with a return statement, to a variable.
data Assignment
  = ExprAssignment {range :: Range, expr :: Expression}
  | BlockAssignment {range :: Range, stmts :: [Statement]}

instance Formattable Assignment where
  formatBare uc (ExprAssignment range expr) = sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "ExprAssignment") (format uc 1 expr)
  formatBare uc (BlockAssignment range stmts) =
    sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "BlockAssignment") (List.intercalate "\n" $ map (format uc 1) stmts)

instance GetAssignments Assignment where
  getAssignments (BlockAssignment {stmts}) = concat $ getAssignments <$> stmts
  getAssignments _ = []

-- | A single statement.
data Statement
  = PrintStr {range :: Range, string :: StringLiteral}
  | PrintExpr {range :: Range, expr :: Expression}
  | Assignment {range :: Range, var :: Variable, assmt :: Assignment}
  | Return {range :: Range, expr :: Expression}

instance Formattable Statement where
  formatBare uc (PrintStr range string) = sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "PrintStr") (format uc 1 string)
  formatBare uc (PrintExpr range expr) = sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "PrintExpr") (format uc 1 expr)
  formatBare uc (Assignment range var assmt) =
    sfmt (str % " " % str % "\n" % str % "\n" % str) (formatBare uc range) (bold uc "Assignment") (format uc 1 var) (format uc 1 assmt)
  formatBare uc (Return range expr) = sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "Return") (format uc 1 expr)

instance GetAssignments Statement where
  getAssignments (Assignment {var, assmt}) = (var, assmt) : getAssignments assmt
  getAssignments _ = []

-- | A program, containing all statements and additional information about memory layout.
data Program = Program
  { range :: Range
  , stmts :: [Statement]
  , strings :: Map Text Index
  , variablesSize :: Offset
  , variablesCount :: Index
  }

instance Formattable Program where
  formatBare uc (Program range stmts strings vs vc) =
    sfmt
      (str % " " % str % "\n" % str % "\nstrings count: " % str % " variables size: " % str % ", variables count: " % str)
      (formatBare uc range)
      (bold uc "Program")
      (List.intercalate "\n" $ map (format uc 1) stmts)
      (color uc 35 $ show $ Map.size strings)
      (color uc 35 $ show vs)
      (color uc 35 $ show vc)

instance GetAssignments Program where
  getAssignments (Program {stmts}) = concat $ getAssignments <$> stmts

-- | Type information about a project, including its name.
data ProjectType = Executable {range :: Range, name :: Text}

instance Formattable ProjectType where
  formatBare uc (Executable range name) =
    sfmt (str % " " % str % " " % str) (formatBare uc range) (bold uc "Executable") (color uc 35 (T.unpack name))

-- | A project is the top level type being run, containing type project type, and the program to be run.
data Project = Project {projectType :: ProjectType, program :: Program}

instance Formattable Project where
  formatBare uc (Project pType program) =
    sfmt (str % "\n" % str % "\n" % str) (bold uc "Project") (format uc 1 pType) (format uc 1 program)

{- UnheckedProject -}

-- | Unchecked version of StringLiteral.
data UncheckedStringLiteral = UncheckedStringLiteral {range :: Range, string :: Text}

instance Formattable UncheckedStringLiteral where
  formatBare uc (UncheckedStringLiteral range string) =
    sfmt (str % " " % str % " \"" % str % "\"") (formatBare uc range) (bold uc "UncheckedStringLiteral") (color uc 35 $ T.unpack string)

-- | Unchecked version of Variable.
data UncheckedVariable = UncheckedVariable {range :: Range, name :: Text}

instance Formattable UncheckedVariable where
  formatBare uc (UncheckedVariable range name) =
    sfmt (str % " " % str % " " % str) (formatBare uc range) (bold uc "UncheckedVariable") (color uc 35 $ T.unpack name)

-- | Unchecked version of Expression.
data UncheckedExpression
  = UIntLiteral {range :: Range, int :: Int}
  | UVarExpr {range :: Range, var :: UncheckedVariable}
  | UBinary {range :: Range, op :: Operator, left :: UncheckedExpression, right :: UncheckedExpression}

instance Formattable UncheckedExpression where
  formatBare uc (UIntLiteral range int) =
    sfmt (str % " " % str % " " % str) (formatBare uc range) (bold uc "UIntLiteral") (color uc 35 $ show int)
  formatBare uc (UVarExpr range var) =
    sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "UVarExpr") (format uc 1 var)
  formatBare uc (UBinary range op left right) =
    sfmt (str % " " % str % "\n" % str % "\n" % str % "\n" % str) (formatBare uc range) (bold uc "UBinary") (format uc 1 op) (format uc 1 left) (format uc 1 right)

-- | Unchecked version of Assignment.
data UncheckedAssignment
  = UExprAssignment {range :: Range, expr :: UncheckedExpression}
  | UBlockAssignment {range :: Range, stmts :: [UncheckedStatement]}

instance Formattable UncheckedAssignment where
  formatBare uc (UExprAssignment range expr) =
    sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "UExprAssignment") (format uc 1 expr)
  formatBare uc (UBlockAssignment range stmts) =
    sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "UBlockAssignment") (List.intercalate "\n" $ map (format uc 1) stmts)

-- | Unchecked version of Statement.
data UncheckedStatement
  = UPrintStr {range :: Range, string :: UncheckedStringLiteral}
  | UPrintExpr {range :: Range, expr :: UncheckedExpression}
  | UAssignment {range :: Range, var :: UncheckedVariable, assmt :: UncheckedAssignment}
  | UReturn {range :: Range, expr :: UncheckedExpression}

instance Formattable UncheckedStatement where
  formatBare uc (UPrintStr range string) =
    sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "UPrintStr") (format uc 1 string)
  formatBare uc (UPrintExpr range expr) =
    sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "UPrintExpr") (format uc 1 expr)
  formatBare uc (UAssignment range var assmt) =
    sfmt (str % " " % str % "\n" % str % "\n" % str) (formatBare uc range) (bold uc "UAssignment") (format uc 1 var) (format uc 1 assmt)
  formatBare uc (UReturn range expr) =
    sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "UReturn") (format uc 1 expr)

-- | Indicates whether an UncheckedStatement is a return statement.
isReturn :: UncheckedStatement -> Bool
isReturn (UReturn _ _) = True
isReturn _ = False

-- | Unchecked version of Program.
data UncheckedProgram = UncheckedProgram {range :: Range, stmts :: [UncheckedStatement]}

instance Formattable UncheckedProgram where
  formatBare uc (UncheckedProgram range stmts) =
    sfmt (str % " " % str % "\n" % str) (formatBare uc range) (bold uc "UncheckedProgram") (List.intercalate "\n" $ map (format uc 1) stmts)

-- | Unchecked version of Project.
data UncheckedProject = UncheckedProject {projectType :: ProjectType, program :: UncheckedProgram}

instance Formattable UncheckedProject where
  formatBare uc (UncheckedProject pType prog) =
    sfmt (str % "\n" % str % "\n" % str) (bold uc "UncheckedProject") (format uc 1 pType) (format uc 1 prog)
