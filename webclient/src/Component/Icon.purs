module Homotopy.Webclient.Component.Icon (icon) where

import Prelude
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.SVG as SVG
import Concur.React.Props as P
import React.DOM as ReactDOM

foreign import featherIcons :: String

use :: forall a. Array (P.ReactProps a) -> Widget HTML a
use p = D.el' (ReactDOM.mkDOM (ReactDOM.IsDynamic false) "use") p []

icon :: forall a. String -> Array (P.ReactProps a) -> Widget HTML a
icon name props =
  SVG.svg (props <> props')
    [ use [ P.unsafeMkProp "xlinkHref" (featherIcons <> "#" <> name) ]
    ]
  where
  props' =
    [ P.width "16"
    , P.height "16"
    , P.fill "none"
    , P.stroke "currentColor"
    , P.strokeWidth 2
    , P.unsafeMkProp "strokeLinecap" "round"
    , P.unsafeMkProp "strokeLinejoin" "round"
    ]
