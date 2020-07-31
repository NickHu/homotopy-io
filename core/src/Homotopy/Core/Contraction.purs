module Homotopy.Core.Contraction where

import Control.Comonad.Cofree (Cofree, buildCofree)
import Control.Comonad.Cofree as C
import Control.MonadZero (guard)
import Control.MonadZero as MZ
import Data.Array ((..))
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldl, maximumBy, minimumBy)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph, areConnected, children, edges, empty, fromMap, insertEdge, insertVertex, parents, stronglyConnectedComponents, toMap, unfoldGraph)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, keys, values, lookup, filterKeys)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tree (Tree)
import Data.Tuple (Tuple(..), fst, snd, swap)
import Homotopy.Core.Common (Boundary(..), Generator(..), Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram(..), DiagramN, cospans, dimension, singularSlices, size, sliceAt, source, toDiagramN, unsafeMake)
import Homotopy.Core.Rewrite (Cospan, Rewrite(..), conePad, makeRewriteN, singularImage)
import Homotopy.Core.Rewrite as R
import Partial.Unsafe (unsafePartial)
import Prelude (class Ord, Unit, bind, compare, const, discard, map, pure, unit, ($), (&&), (+), (-), (/=), (<$>), (<<<), (<>), (==))
import Record (merge, set)

-- | Type to identify a singular height inside a diagram.
type SingularHeight
  = Int

-- | Given a $n$-diagram `d`, and a singular height `i`, try to contract the
-- | singular levels `i` and `i+1` together, returning a rewrite `d
-- | \xrightarrow{r} d'`, where the contracted diagram is `d'`.
-- |
-- | In the spirit of sparsity, `d'` is not returned. To obtain it, call
-- | `rewriteForward r (DiagramN d)`.
contract :: Partial => DiagramN -> SingularHeight -> Maybe Bias -> Maybe Rewrite
contract d i b = do
  s0 <- sliceAt d (Interior (Singular i))
  s1 <- sliceAt d (Interior (Singular (i + 1)))
  r1 <- sliceAt d (Interior (Regular (i + 1)))
  let
    cspans = cospans d
  pre <- cspans L.!! i
  post <- cspans L.!! (i + 1)
  climit <- colimit [ s0, s1 ] [ { leg0: pre.backward, leg1: post.forward, diagram: r1, target0: 0, target1: 1 } ] b
  c0 <- climit.maps A.!! 0
  c1 <- climit.maps A.!! 1
  let
    rewrite =
      makeRewriteN (dimension $ DiagramN d)
        ( { index: i
          , source: pre : post : Nil
          , target: { forward: pre.forward <> c0, backward: post.backward <> c1 }
          , slices: c0 : c1 : Nil
          }
            : Nil
        )
  pure rewrite

-- | Symmetry breaker to obtain biased colimits which may not exist otherwise.
data Bias
  = LeftBias
  | RightBias

-- | For an $n$-diagram, identify a particular subslice at dimension $i$, for
-- | $i < n$.
type Path
  = List SliceIndex

-- | Given an $n$-diagram `d`, a path `p` (which identifies a subslice `d[p]`
-- | at some dimension $j < n$), and a singular height `i`, try to contract the
-- | singular levels `i` and `i+1` of `d[p]` together, and then propagate this
-- | contraction up to dimension $n$, returning the corresponding rewrite `d →
-- | d'`.
propagatedContract :: Partial => DiagramN -> Path -> SingularHeight -> Maybe Bias -> Maybe Rewrite
propagatedContract d Nil i b = contract d i b

propagatedContract d (p : ps) i b = do
  sl <- sliceAt d p
  r <- propagatedContract (toDiagramN sl) ps i b
  let
    dim = dimension $ DiagramN d
  case p of
    Boundary Source -> do
      let
        r' =
          makeRewriteN dim
            $ { index: 0
              , source: Nil
              , target: { forward: R.identity (dim - 1), backward: r }
              , slices: Nil
              }
            : Nil
      pure r'
    Interior (Regular j) -> do
      let
        r' =
          makeRewriteN dim
            $ { index: j
              , source: Nil
              , target: { forward: r, backward: r }
              , slices: Nil
              }
            : Nil
      pure r'
    Interior (Singular j) -> do
      c@{ forward, backward } <- cospans d L.!! j
      let
        r' =
          makeRewriteN dim
            $ { index: j
              , source: c : Nil
              , target: { forward: r <> forward, backward: r <> backward }
              , slices: r : Nil
              }
            : Nil
      pure r'
    Boundary Target -> do
      let
        r' =
          makeRewriteN dim
            $ { index: L.length $ cospans d
              , source: Nil
              , target: { forward: r, backward: R.identity (dim - 1) }
              , slices: Nil
              }
            : Nil
      pure r'

-- | An extensible type for a span of diagrams.
type Span r
  = { leg0 :: Rewrite
    , leg1 :: Rewrite
    , diagram :: Diagram
    | r
    }

-- | Given a span, extract the sub-span one dimension lower relative to a
-- | singular height of `diagram`.
slice :: Partial => forall r. Span r -> SingularHeight -> Span ()
slice { leg0, leg1, diagram } h =
  { leg0: fromMaybe (R.identity (dim - 1)) $ R.safeSlice leg0 h
  , leg1: fromMaybe (R.identity (dim - 1)) $ R.safeSlice leg1 h
  , diagram: subdiagram
  }
  where
  dim = dimension diagram

  subdiagram = fromJust $ (singularSlices $ toDiagramN diagram) L.!! h

-- | Type to index an `Array Diagram`.
type DiagramIndex
  = Int

-- | Extended span with array indices specifying the target diagrams in some
-- | external `Array Diagram` (see `deltaGraph`).
type IndexedSpan
  = Span ( target0 :: DiagramIndex, target1 :: DiagramIndex )

type Node
  = Tuple DiagramIndex SingularHeight

type Edge
  = { span :: Span ()
    , reverse :: Boolean -- if this one half of a bidirectional edge, so we
    -- don't double-count bidirectional edges
    }

-- | For a list of `diagrams` and `spans` compute a graph as follows:
-- |   * For every singular level of a diagram in `diagrams` there is a node in
-- |     the graph. Consecutive singular levels are connected by a
-- |     unidirectional edge. The nodes are tuples `(i, x)` where `i` is the
-- |     index of the diagram in `diagrams` and `x` is the singular height.
-- |   * Every span connects two elements of `diagrams`. For a span
-- |     `diagrams[i] <- middle -> diagrams[j]` the graph contains
-- |     bidirectional edges `(i, f(x)) <-> (j, g(x))` for every singular level
-- |     `x` of the `middle` diagram, where `f` and `g` are the monotone maps
-- |     of singular heights induced by the legs of the span.
-- |   * Edges in the graph are decorated by a span:
-- |     + for unidirectional edges, this is the intermediate span between the
-- |       two singular levels in that diagram;
-- |     + for bidirectional edges, this is the slice of the connecting span at
-- |       the singular height which generated the edge.
deltaGraph :: Partial => Array DiagramN -> Array IndexedSpan -> Graph Node Edge Unit
deltaGraph ds ss =
  let
    ordinalGraph = foldl (\acc (Tuple di d) -> insertOrdinal di d acc) empty $ ordinals
  in
    foldl (\acc (Tuple (Tuple s t) dec) -> insertEdge s t dec acc) ordinalGraph bidirectionalEdges
  where
  ordinals :: Array (Tuple DiagramIndex DiagramN)
  ordinals = mapWithIndex (\i d -> Tuple i d) ds

  bidirectionalEdges :: Array (Tuple (Tuple Node Node) Edge)
  bidirectionalEdges = do
    s@{ leg0, leg1, diagram, target0, target1 } <- ss
    do
      x <- (0 .. (size (toDiagramN diagram) - 1))
      let
        f = singularImage leg0

        g = singularImage leg1

        e =
          Tuple
            ( Tuple
                (Tuple target0 (f x))
                (Tuple target1 (g x))
            )
            { span: slice s x, reverse: false }
      [ e, map (set (SProxy :: SProxy "reverse") true) (lmap swap e) ]

-- | Given a graph $G$, compute its strongly connected components, and return
-- | it as a topologically sorted `Tree`, or fail if it's a DAG
treeSCCs :: forall k d v. Ord k => Graph k d v -> Maybe (Tree (Graph k d v))
treeSCCs g = do
  -- make sure it's a rooted DAG; we know that it's connected by assumption,
  -- and acyclic because it's a condensation, so all we need to do is find a
  -- root node with no parents and make sure it's unique
  root <- only $ L.fromFoldable $ S.filter (\n -> S.isEmpty $ parents n condensation) $ keys adjacency
  pure
    $ buildCofree
        ( \n ->
            Tuple
              (restrictGraph g (S.filter (\k -> lookup k sccs == Just n) $ keys sccs))
              (L.fromFoldable $ children n condensation)
        )
        root
  where
  sccs :: Map k k
  sccs = stronglyConnectedComponents g

  sccNodes :: List k
  sccNodes = L.nub $ values sccs

  -- graph quotiented by SCCs
  condensation :: Graph k Unit Unit
  condensation =
    unfoldGraph
      sccNodes
      (const unit)
      (\n -> map (\k -> Tuple k unit) $ L.filter (\x -> n /= x && areConnected n x g) $ sccNodes)

  adjacency :: Map k (Tuple Unit (List (Tuple k Unit)))
  adjacency = toMap condensation

  only :: forall a. List a -> Maybe a
  only (x : Nil) = pure x

  only _ = MZ.empty

-- | Check if a `Tree` is actually list-like, and return it in an `Array` if
-- | so.
linearise :: forall a. Tree a -> Maybe (Array a)
linearise t = case C.tail t of
  Nil -> pure $ [ C.head t ]
  (x : Nil) -> (A.cons $ C.head t) <$> linearise x
  _ -> MZ.empty

-- | A type for the colimiting cone for an iterated cospan.
type Colimit
  = { apex :: Diagram, maps :: Array Rewrite }

-- | A colimit where legs are annotated by their source diagram, referred to as
-- | a `Node`
type IndexedColimit
  = { apex :: Diagram, indexedMaps :: Array (Tuple Node Rewrite) }

-- | Compute the `Colimit` of a list of diagrams (of equal dimension) and spans.
-- `colimit` takes a list of `diagrams` and `spans`, and
-- computes the colimit as follows:
--   1. compute the strongly connected components of `deltaGraph diagrams spans`;
--   2. try to linearise the resulting graph, in topological order;
--     3a. if the linearisation fails, the colimit does not exist so return `Nothing`;
--     3b. otherwise, the colimit exists and its `apex` consists of singular
--     heights in the same order as the recursive colimits from the
--     linearisation, with its `maps` determined by combining rewrites in the
--     dimension below by target, obtained from the recursive colimits.
colimit :: Partial => Array Diagram -> Array IndexedSpan -> Maybe Bias -> Maybe Colimit
colimit ds ss b = case dim of
  0 {- base case      -} -> do
    maxD <- maximumBy (\(Diagram0 (Generator { dimension: x })) (Diagram0 (Generator { dimension: y })) -> x `compare` y) ds
    let
      (Diagram0 maxG) = maxD

      (Generator { dimension: top }) = maxG
    guard $ A.length (A.filter (\(Diagram0 (Generator { dimension: d })) -> d == top) ds) == 1
    pure $ { apex: maxD, maps: map (\(Diagram0 g) -> if g == maxG then RewriteI else Rewrite0 { source: g, target: maxG }) ds }
  _ {- recursive case -} -> do
    sccs <- treeSCCs $ deltaGraph (map toDiagramN ds) ss
    orderedSCCs <- case b of
      Nothing -> linearise sccs
      Just (LeftBias) -> pure $ A.nubBy (\g g' -> keys (toMap g) `compare` keys (toMap g')) $ A.fromFoldable sccs
      Just (RightBias) ->
        let -- performs a right-biased tree traversal
          foldr' :: forall a b f. Foldable f => (a -> b -> b) -> b -> Cofree f a -> b
          foldr' f e c = f (C.head c) $ foldl (foldr' f) e $ C.tail c
        in
          pure $ A.nubBy (\g g' -> keys (toMap g) `compare` keys (toMap g')) $ foldr' (A.(:)) [] sccs
    -- suppose that each d in ds is an n diagram, and each span s in ss is an n
    -- span
    --
    -- returns an array of n-1 colimits, in linearisation order
    --
    -- the tip of each colimit is an n-1 diagram; one for each SCC, and
    -- each one corresponds to a singular level in the n diagram of the
    -- apex we are trying to build, in linearisation order
    --
    -- the legs of each colimit are n-1 rewrites, one for each diagram in ds
    colimitSlices <-
      sequence
        $ ado
            scc :: Graph Node Edge Unit <- orderedSCCs
            let
              diagramNodes = A.fromFoldable $ keys $ toMap scc

              -- all the singular levels which map to this scc
              diagrams =
                map
                  ( \(Tuple di h) ->
                      fromJust $ sliceAt (toDiagramN $ fromJust $ ds A.!! di) (Interior (Singular h))
                  )
                  diagramNodes

              spans =
                map
                  ( \(Tuple (Tuple s t) { span }) ->
                      merge
                        { target0: fromJust $ A.elemIndex s diagramNodes
                        , target1: fromJust $ A.elemIndex t diagramNodes
                        }
                        span
                  )
                  $ A.filter (\e -> (snd e).reverse == false)
                  $ A.fromFoldable
                  $ edges scc

              annotateLegs :: Colimit -> IndexedColimit
              annotateLegs { apex, maps } = { apex, indexedMaps: A.zip diagramNodes maps }
            in annotateLegs <$> colimit diagrams spans (MZ.empty {- no bias desired for recursive calls -})
    let
      -- we have the singular heights of the apex, but we need to recover the
      -- cospans; forward rewrites are given by taking the first leg (by
      -- `singularHeightOrdering`) and composing it with the preceding forward
      -- rewrite (the regular level that is the source of this rewrite is
      -- guaranteed by globularity to be the same as the regular level in the
      -- apex); backward rewrites are given similarly, from the last leg
      apexCospans :: List Cospan
      apexCospans =
        let
          singularHeightOrdering (Tuple x _) (Tuple y _) = swap x `compare` swap y

          cspans =
            map
              ( \c ->
                  ( \(Tuple (Tuple (Tuple di sh) r) (Tuple (Tuple di' sh') r')) ->
                      { forward: (fromJust $ cospans (toDiagramN $ fromJust $ ds A.!! di) L.!! sh).forward <> r
                      , backward: (fromJust $ cospans (toDiagramN $ fromJust $ ds A.!! di') L.!! sh').backward <> r'
                      }
                  )
                    $ Tuple (fromJust $ minimumBy singularHeightOrdering c.indexedMaps)
                        (fromJust $ maximumBy singularHeightOrdering c.indexedMaps)
              )
              $ colimitSlices
        in
          L.fromFoldable cspans

      -- given the n-rewrites which map all singular levels of a diagram,
      -- construct an (n+1)-rewrite
      combineRewrites :: List (Tuple Node Rewrite) -> Rewrite
      combineRewrites =
        makeRewriteN dim
          <<< case dim of
              1 ->
                snd
                  <<< foldlWithIndex
                      ( \i (Tuple p acc) (Tuple (Tuple di sh) r) -> case r of
                          Rewrite0 st ->
                            Tuple (p + 1)
                              $ L.snoc acc
                                  { index: p
                                  , source: L.slice sh (sh + 1) $ cospans (toDiagramN $ fromJust $ ds A.!! di)
                                  , target: fromJust $ apexCospans L.!! i
                                  , slices: Rewrite0 st : Nil
                                  }
                          RewriteI -> Tuple (p + 1) acc
                      )
                      (Tuple 0 Nil)
              _ ->
                snd
                  <<< L.foldl (\(Tuple p acc) cs -> Tuple (p + (fromJust $ L.last cs).index + 1) $ acc <> map (conePad (p : Nil)) cs) (Tuple 0 Nil)
                  <<< map (\(Tuple _ (RewriteN r)) -> r.cones)
    pure
      $ { apex:
            DiagramN
              $ unsafeMake
                  -- globularity guarantees this regular level is the same as
                  -- the one in the apex
                  (source $ toDiagramN d0)
                  apexCospans
        , maps:
            A.mapWithIndex
              ( \i d ->
                  combineRewrites
                    -- at this point, we have an array of rewrites into apex for each singular slice of d
                    ( L.fromFoldable
                        $ A.sortBy (\(Tuple (Tuple _ x) _) (Tuple (Tuple _ y) _) -> x `compare` y)
                        $ A.filter (\(Tuple (Tuple di _) _) -> i == di)
                        $ A.concatMap (_.indexedMaps) colimitSlices
                    )
              )
              ds
        }
  where
  d0 :: Diagram
  d0 = fromJust $ A.head ds

  dim :: Int
  dim = dimension $ d0

-- | Insert a sequence of nodes of a `diagram` corresponding to its singular
-- | levels into a graph, with every node tagged by its first argument, and
-- | edges between successive singular levels. Edges are decorated by spans
-- | connecting singular levels.
-- |
-- | E.g.\ suppose that a diagram `d` has 3 singular levels, then
-- | `insertOrdinal i d graph` inserts `(i, 0) -> (i, 1) -> (i, 2)` into
-- | `graph`, with the first edge decorated by the span $s₀ \xleftarrow{b₀} r₁
-- | \xrightarrow{f₁} s₁$, the second by $s₁ \xleftarrow{b₁} r₂
-- | \xrightarrow{f₂} s₂$, and the third by $s₂ \xleftarrow{b₂} r₃
-- | \xrightarrow{f₃} s₃$.
insertOrdinal :: DiagramIndex -> DiagramN -> Graph Node Edge Unit -> Graph Node Edge Unit
insertOrdinal di d g =
  foldl
    ( \acc i ->
        insertEdge
          (Tuple di (i - 1))
          (Tuple di i)
          ( { span:
                unsafePartial
                  $ { leg0: (fromJust (cs L.!! (i - 1))).backward
                    , leg1: (fromJust (cs L.!! (i))).forward
                    , diagram: (fromJust $ sliceAt d (Interior (Regular i)))
                    }
            , reverse: false
            }
          )
          acc
    )
    withVertices
    $ fromMaybe []
    $ A.tail (0 .. (height - 1))
  where
  cs = cospans d

  height = size d

  withVertices = foldl (\acc key -> insertVertex key unit acc) g $ map (\i -> Tuple di i) (0 .. (height - 1))

-- | Restrict a graph to a set of nodes; every node not in the set, and every
-- | edge not incident to the set, is discarded.
restrictGraph :: forall k d v. Ord k => Graph k d v -> Set k -> Graph k d v
restrictGraph g ns = fromMap $ map (\(Tuple v adj) -> Tuple v (L.filter (\a -> fst a `S.member` ns) adj)) $ filterKeys (_ `S.member` ns) $ toMap g
