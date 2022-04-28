module Compiler where

import CompilerFlag (BuildFlag)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Set (Set)
import Data.Text (Text)
import Project (Program)

newtype CompilerMonad a = CompilerMonad {compilerAction :: ReaderT FilePath IO a} deriving (Functor, Applicative, Monad)

class CompilerM m where
  writeToFile :: String -> m ()
  writeConsole :: String -> m ()

instance MonadIO CompilerMonad where
  liftIO action = CompilerMonad (lift action)

instance MonadReader FilePath CompilerMonad where
  ask = CompilerMonad ask
  local _ = id

instance CompilerM CompilerMonad where
  writeToFile str = do
    fileName <- ask
    liftIO $ Prelude.writeFile fileName str
  writeConsole = liftIO . putStrLn

writeX86_64_LinuxAsm :: FilePath -> Program -> Set BuildFlag -> IO ()
writeX86_64_LinuxAsm file program flags = runReaderT (compilerAction $ writeImpl program) file

writeImpl :: CompilerM m => Monad m => Program -> m ()
writeImpl program = do
  writeToFile "Hey!"
  writeConsole "Test"
