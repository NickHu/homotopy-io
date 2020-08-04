module Homotopy.Webclient.Components.Diagram.SVG1D where

import Prelude
import Data.Foldable (fold)
import Data.Int (toNumber)
import Data.List ((:))
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Homotopy.Core.Common (Generator, SliceIndex(..), Height(..), Boundary(..))
import Homotopy.Core.Diagram (Diagram(..))
import Homotopy.Core.Diagram as Diagram
import React.Basic (JSX)
import React.Basic.DOM.SVG as SVG

type Props
  = { diagram :: Diagram
    , style :: Style
    , scale :: { x :: Number, y :: Number }
    , colors :: Map Generator String
    }

type Style
  = { wireThickness :: Number
    , pointRadius :: Number
    }

svg1d :: Partial => Props -> JSX
svg1d props =
  SVG.svg
    { children: blocks 0 slices
    , height: show height
    , width: show (2.0 * props.scale.x)
    , viewBox: "0 " <> show (-props.scale.y) <> " " <> show (props.scale.x * 2.0) <> " " <> show height
    }
  where
  diagram = Diagram.toDiagramN props.diagram

  size = Diagram.size diagram

  slices =
    NEL.toList
      $ map toGenerator
      $ Diagram.slices diagram

  height = heightToY (Boundary Source) - heightToY (Boundary Target)

  heightToY = case _ of
    Boundary Source -> (toNumber size * 2.0 + 1.0) * props.scale.y
    Boundary Target -> (-1.0) * props.scale.y
    Interior (Singular i) -> (toNumber (size - i) * 2.0 - 1.0) * props.scale.y
    Interior (Regular i) -> (toNumber (size - i) * 2.0) * props.scale.y

  getColor generator = fromJust $ Map.lookup generator props.colors

  blocks i = case _ of
    r0 : s : r1 : ss -> block i r0 s r1 <> blocks (i + 1) (r1 : ss)
    _ -> []

  block i r0 s r1 =
    [ SVG.path
        { d:
            fold
              [ "M " <> show props.scale.x <> " " <> show (heightToY (Interior (Regular i)))
              , "L " <> show props.scale.x <> " " <> show (heightToY (Interior (Singular i)))
              ]
        , stroke: getColor r0
        , strokeWidth: show props.style.wireThickness
        }
    , SVG.path
        { d:
            fold
              [ "M " <> show props.scale.x <> " " <> show (heightToY (Interior (Regular (i + 1))))
              , "L " <> show props.scale.x <> " " <> show (heightToY (Interior (Singular i)))
              ]
        , stroke: getColor r1
        , strokeWidth: show props.style.wireThickness
        }
    , SVG.circle
        { r: show props.style.pointRadius
        , cx: show props.scale.x
        , cy: show $ heightToY (Interior (Singular i))
        , fill: getColor s
        }
    ]

padWithBoundary :: forall a. NEL.NonEmptyList a -> NEL.NonEmptyList a
padWithBoundary list = NEL.singleton (NEL.head list) <> list <> NEL.singleton (NEL.last list)

toGenerator :: Partial => Diagram -> Generator
toGenerator (Diagram0 generator) = generator
