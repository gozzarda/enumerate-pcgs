module Main where

import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import PCGSat (Vertex, Edge, Graph, Seq, pcgSat, pcgSeqs)

main :: IO ()
main = interact $ const $ unlines [show result]

result :: Maybe Seq
result = listToMaybe $ pcgSeqs graph
-- result = pcgSat graph

edgeVertices :: Set Edge -> Set Vertex
edgeVertices es = Set.union (Set.map fst es) (Set.map snd es)

graph :: Graph
graph = (edgeVertices edges, edges)

undirected :: Set Edge -> Set Edge
undirected es = Set.union es $ Set.map swap es

edges :: Set Edge
edges = threePrism

threePrism :: Set Edge
threePrism = undirected $ Set.fromList
  [ (0, 1)
  , (0, 3)
  , (0, 5)
  , (1, 2)
  , (1, 4)
  , (2, 3)
  , (2, 4)
  , (3, 5)
  , (4, 5) ]

wikipedia :: Set Edge
wikipedia = undirected $ Set.fromList
  [ (0, 1)
  , (0, 5)
  , (1, 2)
  , (1, 4)
  , (2, 3)
  , (2, 4)
  , (2, 5)
  , (3, 4)
  , (4, 5) ]