module BoardSpec (spec) where
import Test.Hspec
import Board

spec :: Spec
spec = do
  describe "initBoard" $ do
    it "returns a square-length board of a given size" $ do
      length (initBoard 3) `shouldBe` 9
      length (initBoard 9) `shouldBe` 81

    it "returns a board of all Empty" $
      all (==Empty) (initBoard 3) `shouldBe` True

  describe "size" $
    it "returns the size that a board with initialized with" $ do
      size (initBoard 3) `shouldBe` 3
      size (initBoard 9) `shouldBe` 9

  describe "result" $ do
    context "when the given board is an empty list" $
      it "returns an empty list" $
        result [] 0 X `shouldBe` []

    context "when the given index is 0" $
      it "returns the result of replacing a Symbol at the head of Board" $
        result [Empty] 0 X `shouldBe` [X]
  

    context "when the given index is not 0" $
      it "returns the result of replacing a Symbol at the given index, with the given Symbol" $
        result [Empty, Empty, Empty] 2 X `shouldBe` [Empty, Empty, X]
  
  describe "tie" $ do
    context "when the board is not complete" $
      it "returns false" $
        tie [X, X, O, O, Empty, Empty, Empty, Empty, X] `shouldBe` False

    context "when a player has won" $
      it "returns false" $
        tie [X, X, X, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` False

    context "when there is a tie game" $
      it "returns true" $ 
       tie [X, X, O, O, O, X, X, O, X] `shouldBe` True

  describe "win" $ do
    context "when the board DOES NOT contain a winner" $
      it "returns false" $
       win [X, X, O, O, O, X, X, O, X] `shouldBe` False

    context "when the board DOES contain a winner, X, along a row" $
      it "returns true" $
        win [X, X, X, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` True 

    context "when the board DOES contain a winner, O, along a row" $
      it "returns true" $
        win [O, O, O, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` True

    context "when the board DOES contain a winner, along a column" $
      it "returns true" $
        win [Empty, X, Empty, Empty, X, Empty, Empty, X, Empty] `shouldBe` True

    context "when the board DOES contain a winner, along a diagonal" $
      it "returns true" $
        win [X, Empty, Empty, Empty, X, Empty, Empty, Empty, X] `shouldBe` True

    context "when the board DOES contain a winner, along a diagonal" $
      it "returns true" $
        win [Empty, Empty, X, Empty, X, Empty, X, Empty, Empty] `shouldBe` True

  describe "spaceIsAvailable" $ do
    context "when the space IS Empty" $ 
      it "returns True" $
        spaceIsAvailable [Empty] 0 `shouldBe` True

    context "when the space IS NOT Empty" $
      it "returns False" $
        spaceIsAvailable [X] 0 `shouldBe` False

    context "when the index IS NOT within the board range" $
      it "returns False" $
        spaceIsAvailable [X] 1 `shouldBe` False