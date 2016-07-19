import Test.Hspec
import Expression

main = hspec $ do
    let e1 = generate "+*221"
    let e2 = generate "-*221"
    describe "Expession tree tests" $ do
        it "generates correct structures" $ do
            shouldBe (show $ e1) "Node Add (Node Multiply (Value 2) (Value 2)) (Value 1)"
            shouldBe (show $ e2) "Node Subtract (Node Multiply (Value 2) (Value 2)) (Value 1)"

        it "evaluates the right result" $ do
            shouldBe (evaluate e1) 5
            shouldBe (evaluate e2) 3