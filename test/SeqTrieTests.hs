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
  describe "branches" $ do
    it "returns [] for the empty trie" $ do
      ST.branches ST.empty `shouldBe` []

    it "returns the prefix elements of a known trie" $ do
      ST.branches simpletrie `shouldBe` [1, 2]

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

  describe "prefixSubtrie" $ do
    it "empty prefix gives whole trie" $ do
      ST.prefixSubtrie [] simpletrie `shouldBe` simpletrie

    it "returns a subtrie of a known trie" $ do
      ST.prefixSubtrie [2, 2] simpletrie `shouldBe` ST.fromList [[1]]

  describe "deleteSubtrie" $ do
    it "deletes a subtrie of a known trie" $ do
      ST.deleteSubtrie 2 simpletrie `shouldBe` ST.fromList [[1, 3], [1, 2], []]

  describe "deletePrefix" $ do
    it "empty prefix deletes whole trie" $ do
      ST.deletePrefix [] simpletrie `shouldBe` ST.empty

    it "deletes a subtrie of a known trie" $ do
      ST.deletePrefix [2, 2] simpletrie `shouldBe` ST.fromList [[2, 3], [1, 3], [1, 2], []]

  describe "union" $ do
    it "merges two know tries" $ do
      let
        lt = ST.fromList [[2, 3], [2, 2, 1]]
        rt = ST.fromList [[1, 3], [1, 2], []]
      ST.union lt rt `shouldBe` simpletrie

    it "finds the union of a trie with itself to be itself" $ do
      ST.union simpletrie simpletrie `shouldBe` simpletrie

    it "the empty trie is the union identity" $ do
      ST.union simpletrie ST.empty `shouldBe` simpletrie