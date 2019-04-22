module SeqTrie
  ( SeqTrie, Seq
  , branches
  , empty, singleton
  , fromList, toList
  , null, size, member
  , insert, delete
  , subtrie, prefixSubtrie
  , deleteSubtrie, deletePrefix
  , union
  ) where

import Prelude hiding (null)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')

type Seq = [Int]
-- Trie of Int-sequences.
data SeqTrie = L Bool | T Bool (IntMap SeqTrie) deriving (Eq,Show)

-- If this trie is the end of a sequence
terminal :: SeqTrie -> Bool
terminal (L b) = b
terminal (T b _) = b

-- The children of a trie
children :: SeqTrie -> (IntMap SeqTrie)
children (L _) = IntMap.empty
children (T _ m) = m

-- The possible first elements of a sequence in the trie
branches :: SeqTrie -> [Int]
branches = IntMap.keys . children

-- The emtpy trie
empty :: SeqTrie
empty = L False

-- A trie containing a single sequence
singleton :: Seq -> SeqTrie
singleton s = insert s empty

-- The trie containing the given list of sequences
fromList :: [Seq] -> SeqTrie
fromList ss = foldr insert empty ss

-- A list of the sequences contained in the trie, in sorted order
toList :: SeqTrie -> [Seq]
toList (L b) = if b then [[]] else []
toList (T b m) = IntMap.foldlWithKey f (if b then [[]] else []) m
  where f ss k t = ss ++ (map ((:) k) (toList t))

-- Tests if the trie is empty
null :: SeqTrie -> Bool
null (L False) = True
null _ = False

-- Wrap with Maybe, empty goes to Nothing
maybeNull :: SeqTrie -> Maybe SeqTrie
maybeNull t = if null t then Nothing else Just t

-- Number of sequences in the trie
size :: SeqTrie -> Int
size (L b) = if b then 1 else 0
size (T b m) = IntMap.foldr ((+).size) (if b then 1 else 0) m

-- Test if a sequence appears in the trie
member :: Seq -> SeqTrie -> Bool
member [] t = terminal t
member _ (L _) = False
member (x:xs) (T _ m) = maybe False (member xs) $ IntMap.lookup x m

-- Insert a sequence into the trie
insert :: Seq -> SeqTrie -> SeqTrie
insert [] (L _) = L True
insert [] (T _ m) = T True m
insert (x:xs) (L b) = T b m'
  where m' = IntMap.singleton x (singleton xs)
insert (x:xs) (T b m) = T b m'
  where
    m' = IntMap.alter af x m
    af = Just . (maybe (singleton xs) (insert xs))

-- Remove a sequence from the trie, if it exists
delete :: Seq -> SeqTrie -> SeqTrie
delete [] (L _) = L False
delete [] (T _ m) = T False m
delete (x:xs) (T b m) = if IntMap.null m' then L b else T b m'
  where
    m' = IntMap.update uf x m
    uf = maybeNull . (delete xs)
delete _ l = l

-- The subtrie for the given first sequence element
subtrie :: Int -> SeqTrie -> SeqTrie
subtrie _ (L _) = empty
subtrie x (T _ m) = IntMap.findWithDefault empty x m

-- The subtrie for the given prefix sequence
prefixSubtrie :: Seq -> SeqTrie -> SeqTrie
prefixSubtrie = flip $ foldl' (flip subtrie)

-- Remove any and all sequences that start with the given element
deleteSubtrie :: Int -> SeqTrie -> SeqTrie
deleteSubtrie x (T b m) = T b $ IntMap.delete x m
deleteSubtrie _ l = l

-- Remove any and all sqeuences that start with the given prefix
deletePrefix :: Seq -> SeqTrie -> SeqTrie
deletePrefix [] _ = empty
deletePrefix (x:xs) (T b m) = if IntMap.null m' then L b else T b m'
  where
    m' = IntMap.update uf x m
    uf = maybeNull . (deletePrefix xs)

-- Merge two tries
union :: SeqTrie -> SeqTrie -> SeqTrie
union (L bl) (L br) = L (bl || br)
union l r = T (terminal l || terminal r) $ IntMap.unionWith union (children l) (children r)