O{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
module Control.Monad.Indexed.IO where

import           Komposition.Prelude

class IxMonadIO (m :: k -> k -> * -> *) where
  iliftIO :: forall (i :: k) a. IO a -> m i i a
