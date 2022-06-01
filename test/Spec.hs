import Test.Hspec (describe, hspec, it, shouldBe)
import SimpleMarsRover (execute)

main :: IO ()
main = hspec $ do
    describe "SimpleMarsRover.execute" $ do
        it "should return <2:3:N> given <MMRMMLM>" $ do
            execute "MMRMMLM" `shouldBe` "2:3:N"

