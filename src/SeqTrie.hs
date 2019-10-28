module SeqTrie
  ( SeqTrie, Seq
  , branches
  , empty, singleton
  , fromList, toList
  , null, size, member
  , insert, delete
  , subtrie, prefixSubtrie
  , deleteSubtrie, deletePrefix
  , union, seqSat
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (null)

type Seq = [Int]
-- Trie of Int-sequences. Empty subtries are always pruned away.
data SeqTrie = Gozz Bool (IntMap SeqTrie) deriving (Eq)

-- instance Show SeqTrie where
--   show (Gozz b m) | IntMap.null m = (if b then "T" else "F")
--   show (Gozz b m) = (if b then "T" else "F") ++ "\n" ++ (indent " " cs)
--     where
--       indent s = init . unlines . (map ((++)s)) . lines
--       showchild k e = (show k) ++ ":" ++ (indent " " $ show e)
--       cs = init $ unlines $ IntMap.elems $ IntMap.mapWithKey showchild m

-- instance Show SeqTrie where
--   show = unlines . (map show) . toList

-- If this trie is the end of a sequence
terminal :: SeqTrie -> Bool
terminal (Gozz b _) = b

-- The children of a trie
children :: SeqTrie -> (IntMap SeqTrie)
children (Gozz _ m) = m

-- The possible first elements of a sequence in the trie
branches :: SeqTrie -> [Int]
branches = IntMap.keys . children

-- The empty trie
empty :: SeqTrie
empty = Gozz False IntMap.empty

-- A trie containing a single sequence
singleton :: Seq -> SeqTrie
singleton = flip insert empty

-- The trie containing the given list of sequences
fromList :: [Seq] -> SeqTrie
fromList = foldr insert empty

-- A list of the sequences contained in the trie, in sorted order
toList :: SeqTrie -> [Seq]
toList (Gozz b m) = IntMap.foldlWithKey f (if b then [[]] else []) m
  where f ss k t = ss ++ (map ((:) k) (toList t))

-- Tests if the trie is empty
null :: SeqTrie -> Bool
null = (==) empty

-- Wrap with Maybe, empty goes to Nothing
maybeNull :: SeqTrie -> Maybe SeqTrie
maybeNull t = if null t then Nothing else Just t

-- Number of sequences in the trie
size :: SeqTrie -> Int
size (Gozz b m) = IntMap.foldr ((+).size) (if b then 1 else 0) m

-- Test if a sequence appears in the trie
member :: Seq -> SeqTrie -> Bool
member [] t = terminal t
member (x:xs) (Gozz _ m) = maybe False (member xs) $ IntMap.lookup x m

-- Insert a sequence into the trie
insert :: Seq -> SeqTrie -> SeqTrie
insert [] (Gozz _ m) = Gozz True m
insert (x:xs) (Gozz b m) = Gozz b m'
  where
    m' = IntMap.alter af x m
    af = Just . (maybe (singleton xs) (insert xs))

-- Remove a sequence from the trie, if it exists
delete :: Seq -> SeqTrie -> SeqTrie
delete [] (Gozz _ m) = Gozz False m
delete (x:xs) (Gozz b m) = Gozz b m'
  where
    m' = IntMap.update uf x m
    uf = maybeNull . (delete xs)

-- The subtrie for the given first sequence element
subtrie :: Int -> SeqTrie -> SeqTrie
subtrie x (Gozz _ m) = IntMap.findWithDefault empty x m

-- The subtrie for the given prefix sequence
prefixSubtrie :: Seq -> SeqTrie -> SeqTrie
prefixSubtrie = flip $ foldl' (flip subtrie)

-- Remove any and all sequences that start with the given element
deleteSubtrie :: Int -> SeqTrie -> SeqTrie
deleteSubtrie x (Gozz b m) = Gozz b $ IntMap.delete x m

-- Remove any and all sequences that start with the given prefix
deletePrefix :: Seq -> SeqTrie -> SeqTrie
deletePrefix [] _ = empty
deletePrefix (x:xs) (Gozz b m) = Gozz b m'
  where
    m' = IntMap.update uf x m
    uf = maybeNull . (deletePrefix xs)

-- Merge two tries
union :: SeqTrie -> SeqTrie -> SeqTrie
union l r = Gozz (terminal l || terminal r) $ IntMap.unionWith union (children l) (children r)

helper :: IntMap SeqTrie -> SeqTrie
helper m = Gozz False m

-- Find the set of minimal sequences that contain all of one set of subsequences but none of another
seqSat :: SeqTrie -> SeqTrie -> SeqTrie
seqSat _ exc | terminal exc = empty
seqSat inc@(Gozz True m) _ | IntMap.null m = inc
seqSat inc@(Gozz _ mi) exc@(Gozz _ me) = helper m
  where
    m = IntMap.mapMaybeWithKey uf mi
    uf k ti = maybeNull $ seqSat (si k ti) (se k)
    si k ti = union ti $ d k mi
    se k = union (te k) $ d k me
    te k = IntMap.findWithDefault empty k me
    d k m = helper $ IntMap.delete k m