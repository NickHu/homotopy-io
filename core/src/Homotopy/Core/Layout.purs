module Homotopy.Core.Layout (Point2D(..), Layout, solveLayout, layoutPosition) where

import Data.Int
import Control.Alt (void)
import Control.Monad.State (State, evalState, execState, get, gets, modify)
import Control.MonadPlus (class Monad)
import Data.Enum (enumFromTo)
import Data.Foldable (for_, sum, traverse_)
import Data.List (List(..), length, zip, (:))
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Homotopy.Core.Common (Height(..))
import Homotopy.Core.Diagram (Diagram(..), DiagramN, toDiagramN)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Interval as Interval
import Homotopy.Core.Rewrite as Rewrite
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, class Show, Unit, bind, discard, map, max, min, negate, otherwise, pure, show, unit, ($), (+), (-), (/), (/=), (<), (<<<), (<=), (<>), (>))

newtype Layout
  = Layout (Map Point2D Number)

layoutPosition :: Layout -> Point2D -> Maybe Number
layoutPosition (Layout layout) point = Map.lookup point layout

data Point2D
  = Point2D Height Height

derive instance eqPoint2D :: Eq Point2D

derive instance ordPoint2D :: Ord Point2D

instance showPoint2D :: Show Point2D where
  show (Point2D x y) = "(Point2D " <> show x <> " " <> show y <> ")"

rangeLen :: Int -> Int -> List Int
rangeLen start len
  | len <= 0 = Nil
  | otherwise = start : rangeLen (start + 1) (len - 1)

data Distance a
  = Distance a a

derive instance eqDistance :: Eq a => Eq (Distance a)

derive instance ordDistance :: Ord a => Ord (Distance a)

data Average a
  = Average (List a) a

instance showAverage :: Show a => Show (Average a) where
  show (Average ps q) = "(Average " <> show ps <> " " <> show q <> ")"

derive instance eqAverage :: Eq a => Eq (Average a)

derive instance ordAverage :: Ord a => Ord (Average a)

generateAverageConstraints :: forall m. Partial => Monad m => (Average Point2D -> m Unit) -> DiagramN -> m Unit
generateAverageConstraints f diagram = go 0 (Diagram.cospans diagram) (NEL.toList $ Diagram.slices diagram)
  where
  go index (cospan : cospans) (r0 : s : r1 : slices) = do
    rewriteAverages (Regular index) (Singular index) r0 s cospan.forward
    rewriteAverages (Regular (index + 1)) (Singular index) r1 s cospan.backward
    go (index + 1) cospans (r1 : slices)

  go _ _ _ = pure unit

  rewriteAverages sourceHeight targetHeight source target rewrite = do
    for_ (rangeLen 0 $ Diagram.size $ toDiagramN $ target) \j ->
      let
        sourcePoints =
          map (Point2D sourceHeight <<< Singular)
            $ Interval.toUnfoldable
            $ Rewrite.singularPreimage rewrite j
      in
        f $ Average sourcePoints (Point2D targetHeight (Singular j))
    for_ (rangeLen 0 $ (Diagram.size $ toDiagramN $ source) + 1) \i ->
      let
        targetPoints =
          map (Point2D targetHeight <<< Regular)
            $ Interval.toUnfoldable
            $ Rewrite.regularPreimage rewrite i
      in
        if length targetPoints > 0 then
          f $ Average targetPoints (Point2D sourceHeight (Regular i))
        else
          pure unit

generateDistanceConstraints :: forall m. Partial => Monad m => (Distance Point2D -> m Unit) -> DiagramN -> m Unit
generateDistanceConstraints f diagram =
  for_ (zip heights slices) \(Tuple height slice) ->
    for_ (rangeLen 0 $ Diagram.size (toDiagramN slice)) \i -> do
      f $ Distance (Point2D height (Regular i)) (Point2D height (Singular i))
      f $ Distance (Point2D height (Singular i)) (Point2D height (Regular (i + 1)))
  where
  heights = enumFromTo (Regular 0) (Regular (Diagram.size diagram))

  slices = NEL.toList $ Diagram.slices diagram

type Constraints
  = { average :: Set (Average Point2D)
    , distance :: Set (Distance Point2D)
    , links :: Map Point2D Point2D
    }

prepareConstraints :: Partial => DiagramN -> Constraints
prepareConstraints diagram =
  execState go
    { average: Set.empty
    , distance: Set.empty
    , links: Map.empty
    }
  where
  go = do
    generateAverageConstraints addAverageConstraint diagram
    generateDistanceConstraints addDistanceConstraint diagram

  follow :: Point2D -> State Constraints Point2D
  follow point = map (fromMaybe point) $ gets (Map.lookup point <<< _.links)

  addAverageConstraint :: Average Point2D -> State Constraints Unit
  addAverageConstraint (Average (p : Nil) q) = do
    target <- follow (min p q)
    void $ modify \s -> s { links = Map.insert (max p q) target s.links }

  addAverageConstraint (Average ps q) = do
    ps' <- traverse follow ps
    q' <- follow q
    void $ modify \s -> s { average = Set.insert (Average ps' q') s.average }

  addDistanceConstraint :: Distance Point2D -> State Constraints Unit
  addDistanceConstraint (Distance p q) = do
    p' <- follow p
    q' <- follow q
    void $ modify \s -> s { distance = Set.insert (Distance p' q') s.distance }

solveConstraints :: Constraints -> Layout
solveConstraints constraints = evalState loop Map.empty
  where
  loop :: State (Map Point2D Number) Layout
  loop = do
    dirty <- step
    if dirty then loop else map finalize get

  finalize :: Map Point2D Number -> Layout
  finalize result =
    Layout
      ( map (\target -> unsafePartial $ fromJust $ Map.lookup target result) constraints.links
          <> result
      )

  step :: State (Map Point2D Number) Boolean
  step = do
    before <- get
    traverse_ propagateDistanceConstraint constraints.distance
    traverse_ propagateAverageConstraint constraints.average
    after <- get
    pure (before /= after)

  propagateDistanceConstraint :: Distance Point2D -> State (Map Point2D Number) Unit
  propagateDistanceConstraint (Distance x y) = do
    xp <- getPosition x
    yp <- getPosition y
    if yp < xp + 1.0 then
      setPosition y (xp + 1.0)
    else
      pure unit

  propagateAverageConstraint :: Average Point2D -> State (Map Point2D Number) Unit
  propagateAverageConstraint (Average xs y) = do
    xps <- traverse getPosition xs
    yp <- getPosition y
    case (sum xps / toNumber (length xps)) - yp of
      diff
        | diff < -0.01 -> for_ (zip xs xps) \(Tuple x xp) -> setPosition x (xp - diff)
        | diff > 0.01 -> setPosition y (yp + diff)
        | otherwise -> pure unit

  getPosition :: Point2D -> State (Map Point2D Number) Number
  getPosition point = do
    position <- gets (Map.lookup point)
    case position of
      Just value -> pure value
      Nothing -> do
        _ <- modify (Map.insert point 0.0)
        pure 0.0

  setPosition :: Point2D -> Number -> State (Map Point2D Number) Unit
  setPosition point pos = void $ modify (Map.insert point pos)

solveLayout :: Diagram -> Maybe Layout
solveLayout (Diagram0 _) = Nothing

solveLayout d
  | Diagram.dimension d < 2 = Nothing

solveLayout (DiagramN d) = Just $ unsafePartial $ solveConstraints $ prepareConstraints d
