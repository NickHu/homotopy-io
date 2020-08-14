module Homotopy.Webclient.Components.Icon
  ( icon
  , IconProps
  , iconButton
  , IconButtonProps
  ) where

import Prelude
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as D
import React.Basic.DOM.SVG as SVG
import React.Basic.Events (handler_)

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

type IconButtonProps
  = { className :: String
    , icon :: String
    , label :: String
    , onClick :: Effect Unit
    }

iconButton :: IconButtonProps -> JSX
iconButton props =
  D.button
    { onClick: handler_ props.onClick
    , className: "icon-button " <> props.className
    , children:
        [ icon
            { className: "icon-button__icon"
            , name: props.icon
            }
        ]
    }
