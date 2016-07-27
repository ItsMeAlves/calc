import Test.Hspec
import Calculate.Expression

main = hspec $ do
    let e1 = fromPrefixNotation "+ * 2 2 1"
    let e2 = fromPrefixNotation "+ * 2 2 6.25"
    let e3 = fromPostfixNotation "2 2 * 1 +"
    let e4 = fromPostfixNotation "2 2 * 6.25 +"
    let e5 = fromInfixNotation "2 * 2 + 1"
    let e6 = fromInfixNotation "2 * 2 + 6.25"
    describe "Expession tree tests" $ do
        it "evaluates the right result" $ do
            shouldBe (evaluate e1) 5
            shouldBe (evaluate e2) 10.25
            shouldBe (evaluate e3) 5
            shouldBe (evaluate e4) 10.25
            shouldBe (evaluate e5) 5
            shouldBe (evaluate e6) 10.25
