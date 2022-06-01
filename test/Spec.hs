import SimpleMarsRover (execute)
import Test.Hspec (describe, hspec, it, shouldBe)

testCases =
  [ ("MMRMMLM", "2:3:N"),
    ("MMMMMMMMMM", "MMMMMMMMMM"),
    ("RMMLM", "2:1:N")
  ]

main :: IO ()
main = hspec $ do
  describe "SimpleMarsRover.execute" $ do
    it "MMRMMLM should be 2:3:N" $ do
      execute "MMRMMLM" `shouldBe` "2:3:N"

    it "MMMMMMMMMM should be MMMMMMMMMM" $ do
      execute "MMMMMMMMMM" `shouldBe` "0:0:N"

    it "RMMLM should be 2:1:N" $ do
      execute "RMMLM" `shouldBe` "2:1:N"