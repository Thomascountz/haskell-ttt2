module ConsoleSpec (spec) where
import Test.Hspec
import Board
import Console

spec :: Spec
spec = do
  describe "boardStr" $ do
    context "when a 3x3 board IS empty" $
      it "returns a string representation of the board" $
        boardStr [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` "\n\
\----------------\n\
\| 00 | 01 | 02 | \n\ 
\----------------\n\
\| 03 | 04 | 05 | \n\ 
\----------------\n\
\| 06 | 07 | 08 | \n\
\----------------\n"

    context "when a 4x4 board IS empty" $
      it "returns a string representation of the board" $
        boardStr [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` "\n\
\---------------------\n\
\| 00 | 01 | 02 | 03 | \n\
\---------------------\n\
\| 04 | 05 | 06 | 07 | \n\
\---------------------\n\
\| 08 | 09 | 10 | 11 | \n\
\---------------------\n\
\| 12 | 13 | 14 | 15 | \n\
\---------------------\n"

    context "when a 3x3 board IS NOT empty" $
      it "returns a string representation of the board" $
        boardStr [X, Empty, O, Empty, X, Empty, O, Empty, X] `shouldBe` "\n\
\----------------\n\
\| XX | 01 | OO | \n\ 
\----------------\n\
\| 03 | XX | 05 | \n\ 
\----------------\n\
\| OO | 07 | XX | \n\
\----------------\n"

  describe "promptMessage" $ do
    context "with a 3x3 board" $
      it "returns a string prompting the user for a coordinate between 0 and 8" $ 
        promptMessage [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` "Please enter a coordinate [0-8]" 

    context "with a 2x2 board" $
      it "returns a string prompting the user for a coordinate between 0 and 3" $ 
        promptMessage [Empty, Empty, Empty, Empty] `shouldBe` "Please enter a coordinate [0-3]" 

  describe "endOfGameMessage" $ do
    context "when the game ends in a TIE" $
      it "returns a 'win' string" $
        endOfGameMessage [X, X, O, O, O, X, X, O, X] `shouldBe` "It's a tie!"
    
    context "when the game ends in a WIN" $
      it "returns a 'tie' string" $
        endOfGameMessage [X, X, X, O, O, X, O, O, X] `shouldBe` "We have ourselves a winner!"

    context "when the game has NOT ENDED" $
      it "returns a 'game not over' string" $
        endOfGameMessage [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` "The game isn't over yet."

  describe "invalidInputMessage" $
    it "returns a string informing the user of bad input" $
      invalidInputMessage "foo" `shouldBe` "\nI'm sorry, I don't understand \"foo\"\n"

  describe "spaceUnavailableMessage" $
    it "returns a string informing the user that a space is not available" $
      spaceUnavailableMessage 0 `shouldBe` "\nI'm sorry, it looks like 0 isn't available.\n" 
