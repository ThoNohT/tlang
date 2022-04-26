module StrFmt (tfmt, sfmt, (%), txt, str, shw, chr, int, nat, itgr) where

import Control.Category (Category (id, (.)))
import Data.String (IsString (fromString))
import qualified Data.Text as T (Text, pack, singleton, unpack)
import Data.Text.Internal.Builder (Builder, fromText, toLazyText)
import Data.Text.Lazy (toStrict)
import Prelude hiding ((.))
import GHC.Natural (Natural)

-- | Simple formatting type based on the format library.
newtype Format r a = Format {runFormat :: (Builder -> r) -> a} deriving (Functor)

instance Semigroup (Format r (a -> r)) where
  Format m <> Format n = Format (\k a -> m (\b1 -> n (\b2 -> k (b1 <> b2)) a) a)

instance Monoid (Format r (a -> r)) where
  mempty = Format (\k _ -> k mempty)

instance (a ~ r) => IsString (Format r a) where
  fromString str = now $ fromString str

instance Category Format where
  id = now mempty
  f . g = f `bind` \a -> g `bind` \b -> now $ a <> b

-- | Concatenate two formatters.
(%) :: Format r a -> Format r' r -> Format r' a
(%) = (.)

infixr 9 %

-- | Monadic indexed bind for holey monoids.
bind :: Format r a -> (Builder -> Format r' r) -> Format r' a
Format m `bind` f = Format $ \k -> m (\a -> runFormat (f a) k)

-- | Constant Builder.
now :: Builder -> Format r r
now a = Format ($ a)

-- | Format a value using a function from this value to a builder.
later :: (a -> Builder) -> Format r (a -> r)
later f = Format (. f)

-- | Convert a Format to a function that takes all parameters of the built up functions and outputs a Text.
tfmt :: Format T.Text a -> a
tfmt m = runFormat m (toStrict . toLazyText)

-- | Convert a Format to a function that takes all parameters of the built up functions and outputs a String.
sfmt :: Format String a -> a
sfmt m = runFormat m (T.unpack . toStrict . toLazyText)

{- Formatters -}

-- | A Text value placeholder.
txt :: Format r (T.Text -> r)
txt = later fromText

-- | A String value placeholder.
str :: Format r (String -> r)
str = later (fromText . T.pack)

-- | A placeholder for any type that implements Show.
shw :: Show a => Format r (a -> r)
shw = later (fromText . T.pack . show)

-- | A Char value placeholder.
chr :: Format r (Char -> r)
chr = later (fromText . T.singleton)

-- | An Int value placeholder.
int :: Format r (Int -> r)
int = shw

-- | A Natural value placeholder.
nat :: Format r (Natural -> r)
nat = shw

-- | An Integer value placeholder.
itgr :: Format r (Integer -> r)
itgr = shw
