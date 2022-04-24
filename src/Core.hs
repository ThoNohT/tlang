module Core (rightPad, leftPad, whileS, whileSE, whileS_, whileSE_, tryLast, tryHead) where

import Control.Monad.Loops (whileM, whileM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State.Lazy as ST

-- | Adds padding of the specified character at the end of the string, until it has at least the specified width.
rightPad :: Char -> Int -> String -> String
rightPad padChar width input = input ++ replicate toPad ' '
 where
  toPad = max 0 $ width - length input

-- | Adds padding of the specified character at the start of the string, until it has at least the specified width.
leftPad :: Char -> Int -> String -> String
leftPad padChar width input = replicate toPad ' ' ++ input
 where
  toPad = max 0 $ width - length input

-- | whileM with the first argument applied to the state.
whileS :: Monad m => (s -> Bool) -> StateT s m a -> StateT s m [a]
whileS a = whileM (a <$> ST.get)

-- | whileM with the first argument applied to the state, and lifted.
whileSE :: MonadTrans t => Monad (t (StateT s m)) => Monad m => (s -> Bool) -> t (StateT s m) a -> t (StateT s m) [a]
whileSE a = whileM (lift $ a <$> ST.get)

-- | whileM_ with the first argument applied to the state.
whileS_ :: Monad m => (s -> Bool) -> StateT s m a -> StateT s m ()
whileS_ a = whileM_ (a <$> ST.get)

-- | whileM_ with the first argument applied to the state, and lifted.
whileSE_ :: MonadTrans t => Monad (t (StateT s m)) => Monad m => (s -> Bool) -> t (StateT s m) a -> t (StateT s m) ()
whileSE_ a = whileM_ (lift $ a <$> ST.get)

-- | Attempts to get the head of a list. Returns Nothing otherwise.
tryHead :: [a] -> Maybe a
tryHead [] = Nothing
tryHead (x : _) = Just x

-- | Attempts to get the last element of a list. Returns Nothing otherwise.
tryLast :: [a] -> Maybe a
tryLast [] = Nothing
tryLast [a] = Just a
tryLast (_ : as) = tryLast as
