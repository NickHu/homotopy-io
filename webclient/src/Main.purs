module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM as RD
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useState, (/\))
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

mkTest :: Component {}
mkTest =
  React.component "Test" \props -> React.do
    state /\ setState <- useState true
    let
      text = if state then "On" else "Off"
    pure
      $ RD.button
          { onClick: handler_ (setState not), children: [ RD.text text ] }

main :: Effect Unit
main = do
  w <- window
  d <- document w
  me <- getElementById "app" (toNonElementParentNode d)
  app <- case me of
    Just e -> pure e
    Nothing -> throw "App element not found."
  test <- mkTest
  RD.render (test {}) app
