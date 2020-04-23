{-# LANGUAGE CPP #-}

---------------------------------------------------------------------------
-- Pattern synonyms exist since ghc 7.8.

-- | BNF Converter: Error Monad.
--
-- Module for backwards compatibility.
--
-- The generated parser now uses @'Either' String@ as error monad.
-- This module defines a type synonym 'Err' and pattern synonyms
-- 'Bad' and 'Ok' for 'Left' and 'Right'.

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bnfc.ErrM where

import Control.Monad       (MonadPlus(..))
import Control.Applicative (Alternative(..))

-- | Error monad with 'String' error messages.
type Err = Either String

pattern Bad msg = Left msg
pattern Ok  a   = Right a

#if __GLASGOW_HASKELL__ >= 808
instance MonadFail Err where
  fail = Bad
#endif

instance Alternative Err where
  empty           = Left "Err.empty"
  (<|>) Left{}    = id
  (<|>) x@Right{} = const x

instance MonadPlus Err where
  mzero = empty
  mplus = (<|>)

