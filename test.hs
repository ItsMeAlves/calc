import Test.Hspec
import Expression

main = hspec $ do
    let e1 = fromPrefixNotation "+*221"
    let e2 = fromPostfixNotation "22*1+"
    let e3 = fromInfixNotation "2*2+1"
    describe "Expession tree tests" $ do
        it "generates correct structures" $ do
            shouldBe (show e1) "Node Add (Node Multiply (Value 2) (Value 2)) (Value 1)"
            shouldBe (show e2) "Node Add (Node Multiply (Value 2) (Value 2)) (Value 1)"
            shouldBe (show e3) "Node Add (Node Multiply (Value 2) (Value 2)) (Value 1)"

        it "evaluates the right result" $ do
            shouldBe (evaluate e1) 5
            shouldBe (evaluate e2) 5
            shouldBe (evaluate e3) 5
