module MinimaxSpec (spec) where
import Test.Hspec
import Board
import Minimax

spec :: Spec 
spec = do 
  describe "player" $ do
    context "when the board is empty" $ do
      it "returns X" $
        player [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` X

      it "returns X" $
        player [Empty, Empty, Empty, Empty] `shouldBe` X

    context "after X has been played" $ do
      it "returns O" $
        player [X, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` O

      it "returns O" $
        player [X, Empty, Empty, Empty] `shouldBe` O

  describe "availableSpaces" $ do
    context "when the board is empty" $
      it "returns indices for all of the spaces" $
        availableSpaces [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8]

    context "when there are only a few spaces left" $
      it "returns indices for all of the Empty spaces" $
        availableSpaces [X, O, X, Empty, Empty, X, O, X, Empty] `shouldBe` [3, 4, 8]

  describe "terminal" $ do
    context "when the game isn't finished" $
      it "returns false" $
        terminal [X, X, O, O, O, X, X, O, Empty] `shouldBe` False

    context "when the game is tied" $
      it "returns true" $
        terminal [X, X, O, O, O, X, X, O, X] `shouldBe` True

    context "when the game is won" $
      it "returns true" $
        terminal [X, X, X, Empty, Empty, Empty, O, O, Empty] `shouldBe` True

  describe "utility" $ do
    context "when the game is tied" $ 
      it "returns 0" $
        utility [X, X, O, O, O, X, X, O, X] `shouldBe` 0

    context "when X wins" $ 
      it "returns -1" $
        utility [X, X, X, O, O, X, O, O, X] `shouldBe` -1

    context "when O wins" $ 
      it "returns 1" $
        utility [O, O, O, X, X, O, X, Empty, X] `shouldBe` 1