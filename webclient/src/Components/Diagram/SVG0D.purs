module Homotopy.Webclient.Components.Diagram.SVG0D where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Homotopy.Core.Common (Generator)
import Homotopy.Core.Diagram (Diagram(..))
import React.Basic (JSX)
import React.Basic.DOM.SVG as SVG

type Props
  = { diagram :: Diagram
    , style :: Style
    , scale :: { x :: Number, y :: Number }
    , colors :: Map Generator String
    }

type Style
  = { pointRadius :: Number
    }

svg0d :: Partial => Props -> JSX
svg0d props =
  SVG.svg
    { children: [ point ]
    , height: show (2.0 * props.scale.y)
    , width: show (2.0 * props.scale.x)
    }
  where
  Diagram0 generator = props.diagram

  point =
    SVG.circle
      { cx: show props.scale.x
      , cy: show props.scale.y
      , r: show props.style.pointRadius
      , fill: fromJust $ Map.lookup generator props.colors
      }
