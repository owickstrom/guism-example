{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module GUISM.Gtk.EventListener where

import           Control.Concurrent.Chan
import           Control.Concurrent.Async       (race)
import qualified GI.Gtk.Declarative.EventSource as Declarative
import qualified GI.Gtk.Declarative.State       as Declarative

data EventListener e = EventListener
  { events      :: Chan e
  , unsubscribe :: IO ()
  }

readEvent :: EventListener e -> IO e
readEvent = readChan . events

subscribeToDeclarativeWidget
  :: Declarative.EventSource s => s e -> Declarative.SomeState -> IO (EventListener e)
subscribeToDeclarativeWidget declWidget st = do
  events       <- newChan
  subscription <- Declarative.subscribe declWidget st (writeChan events)
  pure EventListener { unsubscribe = Declarative.cancel subscription, .. }

raceEvent :: EventListener e -> EventListener e -> IO e
raceEvent a b =
  either pure pure =<< race (readEvent a) (readEvent b)
