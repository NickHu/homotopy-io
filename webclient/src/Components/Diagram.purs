module Homotopy.Webclient.Components.Diagram where

import Prelude
import Data.Map (Map)
import Data.Maybe (fromJust)
import Effect (Effect)
import Homotopy.Core.Common (Generator)
import Homotopy.Core.Diagram (Diagram)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Layout as Layout
import Homotopy.Webclient.Components.Diagram.SVG2D as SVG2D
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM as D
import React.Basic.Hooks as React

type DiagramProps
  = { diagram :: Diagram
    , id :: String
    , scale :: { x :: Number, y :: Number }
    , style2d :: SVG2D.Style
    , colors :: Map Generator String
    }

makeDiagram :: React.Component DiagramProps
makeDiagram = do
  diagram2d <- React.memo makeDiagram2D
  pure \props -> case Diagram.dimension props.diagram of
    0 -> D.text "0-dim"
    1 -> D.text "1-dim"
    _ -> React.element diagram2d props

makeDiagram2D :: Effect (React.ReactComponent DiagramProps)
makeDiagram2D =
  React.reactComponent "Diagram2D" \props -> React.do
    layout <- React.useMemo props.diagram \_ -> Layout.solveLayout props.diagram
    pure $ unsafePartial
      $ SVG2D.svg2d
          { scale: props.scale
          , style: props.style2d
          , colors: props.colors
          , id: props.id
          , diagram: props.diagram
          , layout: fromJust layout
          }
