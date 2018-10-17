module ConsoleSpec (spec) where
import Test.Hspec
import Board
import Console

spec :: Spec
spec = 
  describe "boardStr" $ do
    context "when the board IS empty" $
      it "returns a string representation of the board" $
        boardStr [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] 0 "" `shouldBe` "\n\
\----------------\n\
\| 00 | 01 | 02 | \n\ 
\----------------\n\
\| 03 | 04 | 05 | \n\ 
\----------------\n\
\| 06 | 07 | 08 | \n\
\----------------\n"

    context "when the board IS NOT empty" $
      it "returns a string representation of the board" $
        boardStr [X, Empty, O, Empty, X, Empty, O, Empty, X] 0 "" `shouldBe` "\n\
\----------------\n\
\| XX | 01 | OO | \n\ 
\----------------\n\
\| 03 | XX | 05 | \n\ 
\----------------\n\
\| OO | 07 | XX | \n\
\----------------\n"
