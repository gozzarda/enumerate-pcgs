import Test.Hspec

import qualified SeqTrieTests

main :: IO ()
main = hspec $ do
  describe "SeqTrie" SeqTrieTests.tests