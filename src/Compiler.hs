module Compiler (writeX86_64_LinuxAsm) where

import CompilerFlag (BuildFlag (..), CompilerFlag)
import CompilerFlag qualified as CompilerFlag
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Text qualified as T
import Project (
  Assignment (..),
  Expression (..),
  Index (..),
  Offset (..),
  Operator (..),
  Program (..),
  Statement (..),
  StringLiteral (..),
  UncheckedAssignment (..),
  Variable (..),
  getAssignments,
 )
import StrFmt (int, sfmt, str, txt, (%))

-- | Indents a line with the specified number of whitespaces multiplied by 4.
indent :: Int -> String -> Maybe String
indent i l = Just $ replicate (i * 4) ' ' <> l

-- | Pieces of code that can be flattened to a string.
data Flattenable
  = -- | A line of code.
    C Int String
  | -- | A line of code decoration.
    D Int String
  | -- | A blank line of decoration.
    BL
  | -- | A section that is only included if a condition is matched.
    If Bool [Flattenable]
  | -- | One of two sections that is included depending on a condition.
    IfElse Bool [Flattenable] [Flattenable]
  | -- | A section, that is always included.
    S [Flattenable]

-- | A helper for an often used way to combine multiple Flattenables.
combineLines :: Bool -> [Flattenable] -> Maybe String
combineLines pp ls = Just . intercalate "\n" $ mapMaybe (flatten pp) ls

{- | Flattens a Flattenable to a String, or Nothing if it should be discarded.
 | The Bool indicates whether pretty printing should be used or not.
 | No pretty printing can lead to either no indentation, or lines being entirely discarded.
-}
flatten :: Bool -> Flattenable -> Maybe String
flatten pp (C i l) = indent (if pp then i else (min i 1)) l
flatten pp (D i l) = if pp then indent i l else Nothing
flatten pp BL = if pp then Just "" else Nothing
flatten _ (If False _) = Nothing
flatten pp (If True ls) = combineLines pp ls
flatten pp (IfElse cond t f) = combineLines pp $ if cond then t else f
flatten pp (S ls) = combineLines pp ls

writeX86_64_LinuxAsm :: FilePath -> Program -> Set BuildFlag -> IO ()
writeX86_64_LinuxAsm file program flags = writeFile file $ compile flags program

-- | Creates a string that is compatible with assembly.
asmEncodeString :: T.Text -> String
asmEncodeString txt = if inStr then sfmt (str % "\"") res else res
 where
  (_, inStr, res) = foldl f (True, False, "") $ T.unpack txt
  f (first, prevInStr, acc) c =
    let nowInStr = ord c >= 32 && ord c <= 126
        onSeparator = not first && (not nowInStr || (nowInStr && not prevInStr))
        prefix :: String = if onSeparator then "," else ""
        startQuote :: String = if not prevInStr && nowInStr then "\"" else ""
        endQuote :: String = if prevInStr && not nowInStr then "\"" else ""
        c_ :: String = if nowInStr then [c] else sfmt (" " % int) (ord c)
     in (False, nowInStr, sfmt (str % str % str % str % str) acc endQuote prefix startQuote c_)

-- | Writes the subroutine to use for printing an int64.
writePrintInt64 :: Flattenable
writePrintInt64 =
  S
    [ C 0 "_PrintInt64:"
    , C 1 "sub rsp, 56"
    , C 1 "mov rcx, rdi"
    , C 1 "mov r10, rdi"
    , C 1 "mov r8d, 1"
    , C 1 "mov BYTE [rsp+32], 10"
    , C 1 "neg rcx"
    , C 1 "lea r9, [rsp+32]"
    , C 1 "cmovs rcx, rdi"
    , C 1 "mov rdi, -3689348814741910323"
    , C 0 ".L2:"
    , C 1 "mov rax, rcx"
    , C 1 "mov rsi, r9"
    , C 1 "mul rdi"
    , C 1 "sub rsi, r8"
    , C 1 "shr rdx, 3"
    , C 1 "lea rax, [rdx+rdx*4]"
    , C 1 "add rax, rax"
    , C 1 "sub rcx, rax"
    , C 1 "mov rax, r8"
    , C 1 "add r8, 1"
    , C 1 "add ecx, 48"
    , C 1 "mov BYTE [rsi], cl"
    , C 1 "mov rcx, rdx"
    , C 1 "test rdx, rdx"
    , C 1 "jne .L2"
    , C 1 "test r10, r10"
    , C 1 "jns .L3"
    , C 1 "mov edx, 32"
    , C 1 "sub rdx, r8"
    , C 1 "lea r8, [rax+2]"
    , C 1 "mov BYTE [rsp+rdx], 45"
    , C 0 ".L3:"
    , C 1 "mov eax, 33"
    , C 1 "mov rdx, r8"
    , C 1 "mov edi, 1"
    , C 1 "sub rax, r8"
    , C 1 "lea rsi, [rsp+rax]"
    , C 1 "mov rax, 1"
    , C 1 "syscall"
    , C 1 "add rsp, 56"
    , C 1 "ret"
    ]

-- | Writes an operator in an expression.
writeOp :: Int -> Operator -> Flattenable
writeOp offset (Add {}) = C offset "add rax, rbx"
writeOp offset (Sub {}) = C offset "sub rax, rbx"

-- | Writes an expression. The result of the expression will be on top of the stack.
writeExpression :: Int -> Expression -> Flattenable
writeExpression offset (IntLiteral {value}) =
  S
    [ D offset "; Int literal."
    , C offset $ sfmt ("push " % int) value
    ]
writeExpression offset (VarExpr {var}) =
  let Variable {name, index} = var
   in S
        [ D offset $ sfmt ("; Variable " % txt) name
        , C offset $ sfmt ("call __var_" % int) (coerce index)
        , --  The result of calling the variable will be in rax, put it on the stack.
          C offset "push rax"
        ]
writeExpression offset (Binary {op, left, right}) =
  S
    [ D offset "; Binary operation."
    , writeExpression (offset + 1) left
    , writeExpression (offset + 1) right
    , D offset "; Binary operation, calculate result."
    , C offset "pop rbx"
    , C offset "pop rax"
    , -- Calculate result, and push.
      writeOp offset op
    , C offset "push rax"
    ]

writeStatement :: Int -> Maybe Variable -> Statement -> Flattenable
writeStatement offset _ (PrintStr {string = str_}) =
  let StringLiteral {string, index} = str_
   in S
        [ D offset $ sfmt ("; PrintStr " % str) (asmEncodeString string)
        , C offset $ sfmt ("mov rsi, txt_" % int) (coerce index)
        , C offset $ sfmt ("mov rdx, " % int) (length $ T.unpack string)
        , C offset "mov rax, 1"
        , C offset "mov rdi, 1"
        , C offset "syscall"
        , BL
        ]
writeStatement offset _ (PrintExpr {expr}) =
  S
    [ D offset "; PrintExpr start."
    , writeExpression (offset + 1) expr
    , D offset "; PrintExpr print call."
    , C offset "pop rdi"
    , C offset "call _PrintInt64"
    , BL
    ]
writeStatement offset _ (Assignment {}) = S []
writeStatement offset variable (Return {expr}) =
  S
    [ writeExpression offset expr
    , IfElse
        (isJust variable)
        [C offset "jmp .end"]
        [C offset "jmp _exit"]
    ]

-- | Writes a function that performs an assignment if needed, and returns the value otherwise.
writeAssignmentFunc :: Variable -> Assignment -> Flattenable
writeAssignmentFunc var@Variable {index, name, context, offset} assmt =
  let ctx = context ++ [name]
      varIndex :: Int = coerce $ index
      byteOffset :: Int = varIndex `div` 8
      testBit = 2 ^ (7 - (varIndex `mod` 8))
   in S
        [ D 0 $ sfmt ("; Assignment for variable " % txt % ".") name
        , C 0 $ sfmt ("__var_" % int % ":") (coerce index)
        , -- Check if the variable's init bit is 1.
          D 1 "; Check if the variable was already assigned."
        , C 1 "mov rbx, mem_init"
        , C 1 $ sfmt ("add rbx, " % int) byteOffset
        , C 1 "mov rax, [rbx]"
        , C 1 "mov rcx, rax"
        , -- Set the init bit to 1.
          D 1 "; Set the init bit to 1."
        , C 1 $ sfmt ("or rax, " % int) testBit
        , C 1 "mov [rbx], rax"
        , -- Jump if the init bit was 0.
          C 1 $ sfmt ("test rcx, " % int) testBit
        , D 1 "; Jump to calculate value if it is 0."
        , C 1 "jz .calc"
        , -- If the value is known.
          D 1 "; The value is known, retrieve it."
        , C 1 "mov rbx, mem"
        , C 1 $ sfmt ("add rbx, " % int) (coerce offset)
        , C 1 "mov rax, [rbx]"
        , -- Return.
          C 1 "ret"
        , BL
        , -- Calculate the expression value, it will be on top of the stack.
          C 0 ".calc:"
        , writeAssignment var assmt
        , -- Store the value.
          C 0 ".end:"
        , C 1 "pop rax"
        , D 1 "; Store the value."
        , C 1 "mov rbx, mem"
        , C 1 $ sfmt ("add rbx, " % int) (coerce offset)
        , C 1 "mov [rbx], rax"
        , -- Return.
          C 1 "ret"
        , D 0 ""
        ]

writeAssignment :: Variable -> Assignment -> Flattenable
writeAssignment _ (ExprAssignment {expr}) = writeExpression 1 expr
writeAssignment variable (BlockAssignment {stmts}) =
  S
    [ D 1 "; Block assignment."
    , S $ writeStatement 2 (Just variable) <$> stmts
    , D 1 "; End of block assignment."
    ]

compile :: Set CompilerFlag.BuildFlag -> Program -> String
compile flags (program@Program {stmts, strings}) =
  fromMaybe "" $ flatten prettyPrint $ S [startOfProgram, topLevelStatements, writePrintInt64, dataSection, memorySection]
 where
  prettyPrint = CompilerFlag.isActive PrettyPrintAsm flags

  useNasm = CompilerFlag.isActive UseNasm flags

  startOfProgram =
    S
      [ IfElse
          useNasm
          [ C 0 "Section .text"
          , C 1 "global _start"
          ]
          [ C 0 "format ELF64 executable"
          , C 0 "segment readable executable"
          , C 0 "entry _start"
          ]
      , S
          [ C 0 ""
          , C 0 "_start:"
          , D 0 "; Entry point."
          , BL
          ]
      ]

  topLevelStatements =
    S
      [ S $ writeStatement 1 Nothing <$> stmts
      , S
          [ D 1 "; Exit call. Return number on stack (returned by last statement)."
          , C 0 "_exit:"
          , C 1 "mov rax, 60"
          , C 1 "pop rdi"
          , C 1 "syscall"
          , BL
          ]
      , S
          [ D 1 "; Subroutines."
          , BL
          ]
      , S $ uncurry writeAssignmentFunc <$> (getAssignments program)
      , BL
      ]
  dataSection =
    S
      [ IfElse
          useNasm
          [ C 0 "section .data"
          , S $
              (\(t, Index i) -> C 1 $ sfmt ("txt_" % int % " db " % str) i (asmEncodeString t))
                <$> (Map.toList strings)
          ]
          [ C 0 "segment readable writable"
          , S $
              (\(s, Index i) -> C 1 $ sfmt ("txt_" % int % ": db " % str) i (asmEncodeString s))
                <$> (Map.toList strings)
          ]
      , BL
      ]

  memorySection =
    let -- The init field needs one bit per variable, so we can divide the count by 8, round up and reserve that many
        -- bytes.
        (Offset varSize) = variablesSize program
        (Index varCount) = variablesCount program
        initFieldBytes = (varCount `div` 8) + if varCount `mod` 8 > 0 then 1 else 0
     in If
          (varSize > 0)
          [ IfElse
              useNasm
              [ C 0 "segment .bss"
              , C 1 $ sfmt ("mem_init: resb " % int) initFieldBytes
              , C 1 $ sfmt ("mem: resb " % int) varSize
              ]
              [ C 1 $ sfmt ("mem_init: rb " % int) initFieldBytes
              , C 1 $ sfmt ("mem: rb " % int) varSize
              ]
          ]
