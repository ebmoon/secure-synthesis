import org.scalatest.funspec.AnyFunSpec

class EvalSpec extends AnyFunSpec {

  import Eval._
  import BidirectionalLang._

  describe("Eval") {
    describe("should compute correct post environment for swap") {
      val swap =
        Bind(Variable("x1"), Deref(Variable("x")),
          Bind(Variable("x2"), Deref(Variable("y")),
            Bind(Variable("x3"),
              Assign(Variable("x"), ElimValueToIntroValue(Variable("x2"))),
              Assign(Variable("y"), ElimValueToIntroValue(Variable("x1")))
            )
          )
        )

      it("for an example") {
        val preCxt = Map("x" -> LocValue(100), "y" -> LocValue(101))
        val preEnv = Map(100 -> NumValue(3), 101 -> NumValue(5))
        val postEnvExpected = Map(100 -> NumValue(5), 101 -> NumValue(3))
        val resultValueExpected = UnitValue

        val (postEnv, resultValue) = evaluate(preCxt, preEnv, swap)

        assert(postEnv.equals(postEnvExpected))
        assert(resultValue.equals(resultValueExpected))
      }

      it("for a different example") {
        val preCxt = Map("x" -> LocValue(100), "y" -> LocValue(101))
        val preEnv = Map(100 -> NumValue(7), 101 -> NumValue(5))
        val postEnvExpected = Map(100 -> NumValue(5), 101 -> NumValue(7))
        val resultValueExpected = UnitValue

        val (postEnv, resultValue) = evaluate(preCxt, preEnv, swap)

        assert(postEnv.equals(postEnvExpected))
        assert(resultValue.equals(resultValueExpected))
      }
    }

  }

}
