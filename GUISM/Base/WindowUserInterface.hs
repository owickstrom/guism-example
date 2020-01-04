{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators      #-}
module GUISM.Base.WindowUserInterface where

import           Data.Row.Records
import           Motor.FSM               hiding ( Delete )
import           Data.Kind
import           Data.Typeable

data WindowType = TopWindow | Modal

class WindowUserInterface m where
  type Window m :: WindowType -> Type -> Type
  type WindowMarkup m :: WindowType -> Type -> Type

  newWindow
    :: Typeable event
    => Name n
    -> WindowMarkup m window event
    -> m r (Extend n (Window m window event) r) ()

  patchWindow
    :: Typeable event
    => HasType n (Window m window event) r
    => ((n .== Window m window event) .// r) ~ r
    => Name n
    -> WindowMarkup m window event
    -> m r r ()

  destroyWindow
    :: Typeable e
    => Name n
    -> Actions m '[ n !- Window m window e] r ()

  withNewWindow
    :: ( r' ~ (n .== Window m window event)
       , Typeable event
       )
    => Name n
    -> WindowMarkup m window event
    -> m r' r' a
    -> m r r a

  withNewModalWindow
    :: ( HasType parent (Window m window parentEvent) r
       , r' ~ (modal .== Window m 'Modal event)
       , Typeable event
       )
    => Name parent
    -> Name modal
    -> WindowMarkup m 'Modal event
    -> m r' r' a
    -> m r r a

  nextEvent
    :: HasType n (Window m window e) r
    => Typeable e
    => Name n
    -> m r r e

  beep :: Name n -> m r r ()
