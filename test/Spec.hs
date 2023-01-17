import Lib
import Test.Hspec
import Test.Hspec.QuickCheck()

main :: IO ()
main = hspec $ do
    describe "CptToAst function" $ do
        it "Verify cptToAST work well with (+, *)" $ do
            cptToAST (List[Symbol "+", Symbol "x", Value 1]) `shouldBe` (Just (Call (Astring "+")[Astring "x",Aint 1]))
            cptToAST (List[Symbol "+", Symbol "x", Value 2]) `shouldBe` (Just (Call (Astring "+")[Astring "x",Aint 2]))
            cptToAST (List[Symbol "*", Symbol "tmp", Value 23]) `shouldBe` (Just (Call (Astring "*")[Astring "tmp",Aint 23]))