module SeqTrieTests
  ( tests
  ) where

import Test.Hspec
import Test.QuickCheck

import SeqTrie (SeqTrie)
import qualified SeqTrie as ST
import Data.List (nub,sort)

singletrie :: SeqTrie
singletrie = ST.singleton [1, 2, 3]

simpletrie :: SeqTrie
simpletrie = ST.fromList [[2, 3], [1, 3], [2, 2, 1], [1, 2], []]

tests :: Spec
tests = do
  describe "List conversion" $ do
    it "fromList [] gives the empty trie" $ do
      ST.fromList [] `shouldBe` ST.empty
    it "toList.fromList is equivalent to nub.sort" $ do
      property $ \x -> (ST.toList . ST.fromList) x == (nub.sort) x

  describe "null" $ do
    it "returns True for empty trie" $ do
      ST.null ST.empty `shouldBe` True
    it "returns False for non-empty tries" $ do
      ST.null singletrie `shouldBe` False
      ST.null simpletrie `shouldBe` False
    it "null.fromList is equivalent to null" $ do
      property $ \x -> ST.null (ST.fromList x) == null x

  describe "size" $ do
    it "returns the number of elements in the trie" $ do
      ST.size ST.empty `shouldBe` 0
      ST.size singletrie `shouldBe` 1
      ST.size simpletrie `shouldBe` 5
    it "size.fromList is equivalent to length.nub.sort" $ do
      property $ \x -> (ST.size . ST.fromList) x == (length.nub.sort) x

  describe "member" $ do
    it "tests whether a sequence is an element of the trie" $ do
      ST.member [1, 2, 3] singletrie `shouldBe` True
      ST.member [1, 2, 3] simpletrie `shouldBe` False

    it "returns False for any element in empty" $ do
      property $ \x -> not $ ST.member x ST.empty

    it "returns True for any element in the trie" $ do
      property $ \x -> all ((flip ST.member) (ST.fromList x)) x

  describe "insert" $ do
    it "turns an empty trie into a singleton" $ do
      ST.insert [1, 2, 3] ST.empty `shouldBe` singletrie

    it "makes the new element a member of the trie" $ do
      property $ \x xs -> ST.member x (ST.insert x $ ST.fromList xs)

  describe "delete" $ do
    it "turns a singleton into the empty trie" $ do
      ST.delete [1, 2, 3] singletrie `shouldBe` ST.empty

    it "makes the element not a member of the trie" $ do
      property $ \x xs -> not $ ST.member x $ ST.delete x $ ST.fromList (x:xs)

  describe "subtrie" $ do
    it "returns empty if no sequence starts with the given value" $ do
      ST.subtrie 0 simpletrie `shouldBe` ST.empty

    it "returns a subtrie of a known trie" $ do
      ST.subtrie 2 simpletrie `shouldBe` ST.fromList [[3], [2, 1]]

  -- TODO: prefixSubtrie

  describe "deleteSubtrie" $ do
    it "deletes a subtrie of a known trie" $ do
      ST.deleteSubtrie 2 simpletrie `shouldBe` ST.fromList [[1, 3], [1, 2], []]

  -- TODO: deletePrefix

  describe "union" $ do
    it "merges two know tries" $ do
      ST.union lt rt `shouldBe` simpletrie
        where
          lt = ST.fromList [[2, 3], [2, 2, 1]]
          rt = ST.fromList [[1, 3], [1, 2], []]