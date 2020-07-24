module Homotopy.Webclient.Main where

import Homotopy.Webclient.Diagram2D
import Prelude
import Concur.React.DOM as D
import Concur.React.Run (runWidgetInDom)
import Control.Monad.Rec.Class (forever)
import Data.Array ((!!))
import Data.List (List(..))
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Homotopy.Core.Common (Boundary(..), Generator(..))
import Homotopy.Core.Diagram (Diagram(..), DiagramN)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Layout as Layout
import Partial.Unsafe (unsafePartial)

associativity :: DiagramN
associativity =
  let
    attach b e s l = unsafePartial $ fromJust $ Diagram.attach b e s l

    x = Generator { id: 0, dimension: 0 }

    f = Generator { id: 1, dimension: 1 }

    m = Generator { id: 2, dimension: 2 }

    a = Generator { id: 3, dimension: 3 }

    fd = Diagram.fromGenerator (Diagram0 x) (Diagram0 x) f

    ffd = attach Target Nil fd fd

    md = Diagram.fromGenerator (DiagramN ffd) (DiagramN fd) m

    mfd = attach Target Nil fd md

    ld = attach Target Nil md mfd

    fmd = attach Source Nil fd md

    rd = attach Target Nil md fmd

    ad = Diagram.fromGenerator (DiagramN ld) (DiagramN rd) a
  in
    ad

main :: Effect Unit
main =
  let
    diagram = Diagram.target associativity

    layout = unsafePartial $ fromJust $ Layout.solveLayout diagram

    ctx =
      { colors: \(Generator x) -> unsafePartial $ fromJust $ [ "gray", "black", "red" ] !! x.dimension
      , scale: { x: 50.0, y: 50.0 }
      , id: "diagram"
      , style:
          { pointRadius: 8.0
          , wireThickness: 4.0
          , crossingThickness: 8.0
          }
      }
  in
    runWidgetInDom "app"
      $ forever do
          event <- D.div' [ unsafePartial $ diagramSVG ctx layout diagram ]
          liftEffect (logShow event)
