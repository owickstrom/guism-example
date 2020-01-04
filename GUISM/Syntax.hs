{-# LANGUAGE PolyKinds #-}
module GUISM.Syntax
  ( (>>)
  , (>>=)
  , (>>>=)
  , void
  , return
  )
where

import           Prelude                        ( )
import           Motor.FSM

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (>>>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

void :: IxMonad m => m i i a -> m i i ()
void = (>>> (ireturn ()))

return :: IxMonad m =>a -> m i i a
return = ireturn
