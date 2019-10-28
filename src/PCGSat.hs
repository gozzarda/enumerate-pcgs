module PCGSat
  ( Vertex, Edge
  , Graph, Seq
  , pcgSat, pcgSeqs
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import SeqSat (Seq, satisfiable, solutions)

type Vertex = Int
type Edge = (Int, Int)
type Graph = (Set Vertex, Set Edge)

-- Test whether the given graph is a polygon-circle graph
-- and return a sequence representation of it if one exists
pcgSat :: Graph -> Maybe Seq
pcgSat g = satisfiable inc exc
  where
    inc = includeEdgeSeqs g
    exc = excludeEdgeSeqs g

-- Test whether the given graph is a polygon-circle graph
-- and return a list of sequence representations
pcgSeqs :: Graph -> [Seq]
pcgSeqs g = solutions inc exc
  where
    inc = includeEdgeSeqs g
    exc = excludeEdgeSeqs g


-- Computes a list of subsequences that must appear in the sequence
-- representation of the graph for all its edges to be present
includeEdgeSeqs :: Graph -> [Seq]
includeEdgeSeqs (_, es) = concat $ Set.elems $ Set.map includeEdgeSeq es

-- Computes the pair of subsequences that must appear in the sequence
-- representation of the graph for this edge to be present
includeEdgeSeq :: Edge -> [Seq]
includeEdgeSeq (u, v) = [[u, v, u], [v, u, v]]

-- Computes a list of subsequences that must not appear in the
-- seqeuence representation of the graph to avoid false edges
excludeEdgeSeqs :: Graph -> [Seq]
excludeEdgeSeqs (vs, es) = concat $ Set.elems $ Set.map excludeEdgeSeq es'
  where
    vl = Set.elems vs
    vv = Set.fromList [ (u, v) | u <- vl, v <- vl, u /= v ]
    es' = Set.difference vv es

-- Computes the pair of subsequences that must not appear in the
-- seqeuence representation of the graph to avoid this edge
excludeEdgeSeq :: Edge -> [Seq]
excludeEdgeSeq (u, v) = [[u, v, u, v], [v, u, v, u]]