module Homotopy.Webclient.Components.Diagram where

import Prelude
import Data.Array as Array
import Data.EuclideanRing (mod)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Effect (Effect)
import Homotopy.Core.Common (Boundary(..), Generator, SliceIndex(..), Height(..))
import Homotopy.Core.Diagram (Diagram)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Layout as Layout
import Homotopy.Webclient.Components.Diagram.SVG2D as SVG2D
import Homotopy.Webclient.Components.Diagram.SVG1D as SVG1D
import Homotopy.Webclient.Components.Diagram.SVG0D as SVG0D
import Homotopy.Webclient.Components.Icon (icon)
import Partial.Unsafe (unsafePartial)
import React.Basic (JSX)
import React.Basic.DOM as D
import React.Basic.Events (handler_)
import React.Basic.Hooks as React

type DiagramProps
  = { diagram :: Diagram
    , id :: String
    , scale :: { x :: Number, y :: Number }
    , style2d :: SVG2D.Style
    , style1d :: SVG1D.Style
    , style0d :: SVG0D.Style
    , colors :: Map Generator String
    , onSliceSelect :: SliceIndex -> Effect Unit
    , onClick :: { x :: Int, y :: Int } -> Effect Unit
    }

makeDiagram :: React.Component DiagramProps
makeDiagram = do
  diagram2d <- React.memo makeDiagram2D
  pure \props -> case Diagram.dimension props.diagram of
    0 -> diagram0d props
    1 -> diagram1d props
    _ -> React.element diagram2d props

diagram0d :: DiagramProps -> JSX
diagram0d props =
  let
    diagram =
      unsafePartial
        $ SVG0D.svg0d
            { style: props.style0d
            , colors: props.colors
            , diagram: props.diagram
            , scale: props.scale
            }
  in
    D.div
      { className: "diagram"
      , children:
          [ D.div
              { className: "diagram__content"
              , children: [ diagram ]
              }
          ]
      }

diagram1d :: DiagramProps -> JSX
diagram1d props =
  let
    diagram =
      unsafePartial
        $ SVG1D.svg1d
            { style: props.style1d
            , colors: props.colors
            , diagram: props.diagram
            , scale: props.scale
            }

    sliceControls_ =
      unsafePartial
        $ sliceControls
            { sliceNumber: Diagram.size $ Diagram.toDiagramN props.diagram
            , onSelect: props.onSliceSelect
            }
  in
    D.div
      { className: "diagram"
      , children:
          [ D.div
              { className: "diagram__content"
              , children: [ diagram ]
              }
          , sliceControls_
          ]
      }

makeDiagram2D :: Effect (React.ReactComponent DiagramProps)
makeDiagram2D =
  React.reactComponent "Diagram2D" \props -> React.do
    layout <- React.useMemo props.diagram \_ -> Layout.solveLayout props.diagram
    let
      diagram =
        unsafePartial
          $ SVG2D.svg2d
              { scale: props.scale
              , style: props.style2d
              , colors: props.colors
              , id: props.id
              , onClick: props.onClick
              , diagram: props.diagram
              , layout: fromJust layout
              }

      sliceControls_ =
        unsafePartial
          $ sliceControls
              { sliceNumber: Diagram.size $ Diagram.toDiagramN props.diagram
              , onSelect: props.onSliceSelect
              }
    pure
      $ D.div
          { className: "diagram"
          , children:
              [ D.div
                  { className: "diagram__content"
                  , children: [ diagram ]
                  }
              , sliceControls_
              ]
          }

type SliceControlProps
  = { sliceNumber :: Int
    , onSelect :: SliceIndex -> Effect Unit
    }

sliceControls :: SliceControlProps -> JSX
sliceControls props =
  D.div
    { className: "slice-control"
    , children:
        map sliceButton
          $ Array.reverse
          $ Array.range (-1) (props.sliceNumber * 2 + 1)
    }
  where
  sliceButton i =
    D.div
      { className: "slice-control__button"
      , onClick: handler_ (props.onSelect (indexToHeight i))
      , children:
          [ icon
              { name: if i `mod` 2 == 0 then "chevron-right" else "chevrons-right"
              , className: "slice-control__icon"
              }
          ]
      }

  indexToHeight :: Int -> SliceIndex
  indexToHeight i
    | i < 0 = Boundary Source
    | i > props.sliceNumber * 2 = Boundary Target
    | i `mod` 2 == 0 = Interior (Regular (i / 2))
    | otherwise = Interior (Singular ((i - 1) / 2))
