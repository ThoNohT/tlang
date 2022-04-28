module Compiler where

import CompilerFlag (BuildFlag)
import Data.Set (Set)
import Project (Program)

writeX86_64_LinuxAsm :: String -> Program -> Set BuildFlag -> IO ()
writeX86_64_LinuxAsm = undefined
