{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Prelude                        hiding (return, (>>), (>>=))

import           Control.Monad.Indexed
import           Control.Monad.Indexed.IO
import           Data.Row.Records
import           Data.Typeable                  (Typeable)
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as Vector
import           GHC.Exts
import           GHC.OverloadedLabels
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           Motor.FSM                      hiding ((:=))


import           GUISM.Base.WindowUserInterface
import           GUISM.Gtk
import           GUISM.Gtk.Markup
import           GUISM.Syntax

-- * Application events and states

data HomeEvent = Save | About | Exit deriving (Eq, Typeable)

data AboutEvent = Close deriving (Eq, Typeable)

class AppViews markup where
  homeView :: markup 'TopWindow HomeEvent
  aboutView :: markup 'Modal AboutEvent

type Application t m
  = ( MonadFSM (t m)
    , IxMonadIO (t m)
    , WindowUserInterface (t m)
    , AppViews (WindowMarkup (t m))
    )

start :: Application t m => t m Empty Empty ()
start = do
  info "Starting application"
  withNewWindow #home homeView inHome
  where
    inHome = do
      patchWindow #home homeView
      e <- nextEvent #home
      case e of
          Save -> do
            beep #home
            info "FIXME: Support saving files"
            inHome
          About -> do
            showAbout #home
            inHome
          Exit -> info "Bye."

showAbout
  :: (Application t m, r ~ (parent .== (Window (t m) 'TopWindow event)))
  => Name parent
  -> t m r r ()
showAbout parent = do
  iliftIO (putStrLn "Showing 'About' screen")
  withNewModalWindow parent #about aboutView $ do
    Close <- nextEvent #about
    ireturn ()

-- * GTK views

padded :: Vector (Widget event) -> Widget event
padded ws = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal]
  [ BoxChild
      defaultBoxChildProperties { padding = 10, expand = True, fill = True }
      (container
        Gtk.Box
        [#orientation := Gtk.OrientationVertical, #spacing := 10]
        (Vector.map (BoxChild defaultBoxChildProperties { padding = 10, expand = True, fill = False }) ws)
      )
  ]

instance AppViews GtkWindowMarkup where
  homeView = GtkTopWindowMarkup $ bin Gtk.Window [ on #deleteEvent (const (True, Exit))] $
    container Gtk.Box [#orientation := Gtk.OrientationVertical]
    [ container Gtk.MenuBar []
      [ subMenu
        "File"
        [ menuItem Gtk.MenuItem [on #activate Save] (widget Gtk.Label [#label := "Save"])
        , menuItem Gtk.MenuItem [on #activate Exit] (widget Gtk.Label [#label := "Exit"])
        ]
      , subMenu
        "Help"
        [ menuItem Gtk.MenuItem [on #activate About] (widget Gtk.Label [#label := "About"])
        ]
      ]
    , BoxChild defaultBoxChildProperties { expand = True } (widget Gtk.Label [#label := "Welcome!"])
    ]
  aboutView = GtkModalMarkup $ bin Gtk.Dialog [] $ padded
              [ widget Gtk.Label [#label := "This is a GUISM application."]
              , widget Gtk.Button [#label := "Close", on #clicked Close]
              ]

-- * Extremely basic logging

info :: IxMonadIO m => String -> m i i ()
info = iliftIO . putStrLn

-- * Main

main :: IO ()
main = runGtkUserInterface id start
