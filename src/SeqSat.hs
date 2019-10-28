module SeqSat
  ( Seq
  , satisfiable
  , solutions
  ) where

import Data.Maybe (listToMaybe, catMaybes)
import Data.List (sortBy)
import Data.Function (on)

import SeqTrie (SeqTrie, Seq)
import qualified SeqTrie as ST

import Debug.Trace (trace, traceShow)

-- Remove the given element from the head of all sequences that contain it
consume :: Int -> SeqTrie -> SeqTrie
consume x t = ST.union (ST.subtrie x t) (ST.deleteSubtrie x t)

-- Returns a sequence that includes all the subsequences in the first argument but none from the second, if one exists
satisfiable :: [Seq] -> [Seq] -> Maybe Seq
satisfiable inc exc = satisfiable' [] (ST.fromList inc) (ST.fromList exc)

traceStack :: Int -> [Int] -> a -> a
-- traceStack l s | length s < l = traceShow (reverse s)
-- traceStack l s | length s == l = trace ((show $ reverse s) ++ "...")
traceStack _ _ = id

-- Returns a sequence that includes all the subsequences in the first argument but none from the second, if one exists
satisfiable' :: [Int] -> SeqTrie -> SeqTrie -> Maybe Seq
satisfiable' _ _ exc | ST.member [] exc = Nothing
satisfiable' _ inc _ | null $ ST.branches inc = Just []
satisfiable' s inc exc = traceStack 4 s $ firstJust $ map rec $ ST.branches inc
  where rec x = fmap ((:) x) $ satisfiable' (x:s) (consume x inc) (consume x exc)

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe.catMaybes

-- Returns all minimal solutions
solutions :: [Seq] -> [Seq] -> [Seq]
solutions inc exc = ST.toList $ ST.seqSat (ST.fromList inc) (ST.fromList exc)