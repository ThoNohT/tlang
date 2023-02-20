module Compiler (writeX86_64_LinuxAsm) where

import CompilerFlag (BuildFlag (..), CompilerFlag)
import CompilerFlag qualified as CompilerFlag
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Project (Index (..), Offset (..), Program (..), Statement, Variable, stmts)
import Text.Printf (printf)

-- | Indents a line with the specified number of whitespaces multiplied by 4.
indent :: Int -> String -> Maybe String
indent i l = Just $ replicate (i * 4) ' ' <> l

-- | Pieces of code that can be flattened to a string.
data Flattenable
  = -- A line of code.
    C Int String
  | -- A line of code decoration.
    D Int String
  | -- A blank line of decoration.
    BL
  | -- A section that is only included if a condition is matched.
    If Bool [Flattenable]
  | -- One of two sections that is included depending on a condition.
    IfElse Bool [Flattenable] [Flattenable]
  | -- A section, that is always included.
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
flatten pp (IfElse cond f t) = combineLines pp $ if cond then t else f
flatten pp (S ls) = combineLines pp ls

writeX86_64_LinuxAsm :: FilePath -> Program -> Set BuildFlag -> IO ()
writeX86_64_LinuxAsm file program flags = do
  let outputStr = compile flags program
  writeFile file outputStr

asmEncodeString :: Text -> String
asmEncodeString str = undefined

writePrintInt64 :: Flattenable
writePrintInt64 = undefined

writeStatement :: Int -> Maybe Variable -> Statement -> Flattenable
writeStatement offset variable stmt = undefined

writeAssignment :: Int -> Statement -> Flattenable
writeAssignment offset stmt = undefined

compile :: Set CompilerFlag.BuildFlag -> Program -> String
compile flags program =
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
      [ S $ writeStatement 1 Nothing <$> (stmts program)
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
      , S $ writeAssignment 1 <$> (stmts program)
      , BL
      ]
  dataSection =
    S
      [ IfElse
          useNasm
          [ C 0 "section .data"
          , S $ (\(t, Index i) -> C 1 $ printf "txt_%i db %s" i (asmEncodeString t)) <$> (Map.toList $ strings program)
          ]
          [ C 0 "segment readable writable"
          , S $ (\(s, Index i) -> C 1 $ printf "txt_%i: db %s" i (asmEncodeString s)) <$> (Map.toList $ strings program)
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
              , C 1 $ printf "mem_init: resb %i" initFieldBytes
              , C 1 $ printf "mem: resb %i" varSize
              ]
              [ C 1 $ printf "mem_init: rb %i" initFieldBytes
              , C 1 $ printf "mem: rb %i" varSize
              ]
          ]
