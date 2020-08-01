module Homotopy.Webclient.Components.Icon (icon, IconProps) where

import Prelude
import React.Basic (JSX)
import React.Basic.DOM.SVG as SVG
import React.Basic.Events (EventHandler)

foreign import featherIcons :: String

type IconProps
  = { className :: String
    , name :: String
    }

icon :: IconProps -> JSX
icon props =
  SVG.svg
    { fill: "none"
    , stroke: "currentColor"
    , strokeWidth: "2"
    , strokeLinecap: "round"
    , strokeLinejoin: "round"
    , className: props.className
    , children:
        [ SVG.use
            { xlinkHref: featherIcons <> "#" <> props.name
            }
        ]
    }
