{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module GUISM.Gtk
  ( GtkUserInterface
  , runGtkUserInterface
  )
where

import           Prelude

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad                  (void)
import           Control.Monad.Indexed          ()
import           Control.Monad.Indexed.IO
import           Control.Monad.Indexed.Trans
import           Control.Monad.IO.Class
import           Data.Row.Records               (Empty)
import           Data.String
import           Data.Typeable                  (Typeable)
import           GHC.OverloadedLabels
import qualified GI.Gdk                         as Gdk
import qualified GI.GLib.Constants              as GLib
import qualified GI.Gtk                         as Gtk
import qualified GI.Gtk.Declarative             as Declarative
import qualified GI.Gtk.Declarative.State       as Declarative
import           Motor.FSM                      hiding ((:=))
import qualified Motor.FSM                      as FSM

import           GUISM.Base.WindowUserInterface
import           GUISM.Gtk.EventListener
import           GUISM.Gtk.Markup

newtype GtkUserInterface m i o a = GtkUserInterface
  (FSM m i o a) deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, MonadFSM, IxMonadTrans)

deriving instance Monad m => Functor (GtkUserInterface m i i)
deriving instance Monad m => Applicative (GtkUserInterface m i i)
deriving instance Monad m => Monad (GtkUserInterface m i i)

instance MonadIO m => IxMonadIO (GtkUserInterface m) where
  iliftIO = ilift . liftIO

data GtkWindow window event = GtkWindow
  { markup      :: GtkWindowMarkup window event
  , widgetState :: Declarative.SomeState
  , viewEvents  :: EventListener event
  }

asGtkWindow :: GtkWindow window event -> IO Gtk.Window
asGtkWindow w =
  Declarative.someStateWidget (widgetState w) >>= Gtk.unsafeCastTo Gtk.Window

instance MonadIO m => WindowUserInterface (GtkUserInterface m) where
  type Window (GtkUserInterface m) = GtkWindow
  type WindowMarkup (GtkUserInterface m) = GtkWindowMarkup

  newWindow name markup' = FSM.new name =<<< irunUI
    (do
      s <- Declarative.create markup'
      win <- Gtk.unsafeCastTo Gtk.Window =<< Declarative.someStateWidget s
      -- Set up event listeners
      viewEvents <- subscribeToDeclarativeWidget markup' s
      -- And show recursively as this is a new widget tree
      #showAll win
      return (GtkWindow markup' s viewEvents)
    )

  patchWindow name markup' = FSM.get name >>>= \w ->
    FSM.enter name
      =<<< case Declarative.patch (widgetState w) (markup w) markup' of
             Declarative.Modify f -> irunUI $ do
               s' <- f
               unsubscribe (viewEvents w)
               viewEvents' <- subscribeToDeclarativeWidget markup' s'
               return w { markup      = markup'
                        , widgetState = s'
                        , viewEvents  = viewEvents'
                        }
             Declarative.Replace create' -> irunUI $ do
               Gtk.widgetDestroy =<< asGtkWindow w
               s'  <- create'
               win <- Gtk.unsafeCastTo Gtk.Window
                 =<< Declarative.someStateWidget s'
               viewEvents <- subscribeToDeclarativeWidget markup' s'
               #showAll win
               return (GtkWindow markup' s' viewEvents)
             Declarative.Keep -> ireturn w

  destroyWindow name = FSM.get name >>>= \w ->
    irunUI (Gtk.widgetDestroy =<< asGtkWindow w) >>> FSM.delete name

  withNewWindow name markup action =
    call $ newWindow name markup >>> action >>>= \x ->
      destroyWindow name >>> ireturn x

  withNewModalWindow parent name markup action =
    FSM.get parent >>>= \p ->
      call $
        newWindow name markup
        >>> FSM.get name
        >>>= \w -> irunUI (do
          cw <- asGtkWindow w
          pw <- asGtkWindow p
          Gtk.windowSetModal cw True
          Gtk.windowSetTypeHint cw Gdk.WindowTypeHintDialog
          Gtk.windowSetTransientFor cw (Just pw))
        >>> action
        >>>= \x ->
          destroyWindow name
          >>> ireturn x

  nextEvent name = FSM.get name >>>= (iliftIO . readEvent . viewEvents)

  beep _ = irunUI Gdk.beep

newtype GtkMainExitedException =
  GtkMainExitedException String deriving (Typeable, Show)

instance Exception GtkMainExitedException

runGtkUserInterface' :: Monad m => GtkUserInterface m Empty Empty a -> m a
runGtkUserInterface' (GtkUserInterface a) = FSM.runFSM a

runGtkUserInterface :: Monad m => (m () -> IO ()) -> GtkUserInterface m Empty Empty () -> IO ()
runGtkUserInterface runEffects ui = do
  void $ Gtk.init Nothing
  withAsync (runEffects (runGtkUserInterface' ui) <* Gtk.mainQuit) $ \result -> do
    Gtk.main
    poll result >>= \case
      Nothing ->
        throwIO (GtkMainExitedException "gtk's main loop exited unexpectedly")
      Just (Left  e ) -> throwIO e
      Just (Right ()) -> return ()

-- * IO and GDK threading utilities

runUI_ :: IO () -> IO ()
runUI_ ma = void (Gdk.threadsAddIdle GLib.PRIORITY_HIGH (ma *> return False))

runUI :: IO a -> IO a
runUI ma = do
  ret <- newEmptyMVar
  runUI_ (ma >>= putMVar ret)
  takeMVar ret

irunUI :: IxMonadIO (t m) => IO a -> t m i i a
irunUI = iliftIO . runUI
