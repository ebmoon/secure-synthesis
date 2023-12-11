import org.scalatest.funspec.AnyFunSpec

class SynthSpec extends AnyFunSpec {

  import Eval._

  describe("Synth") {
    it("should synthesize correct swap function") {
      val inputType = Map("x" -> RefType(IntType(High), Low), "y" -> RefType(IntType(High), Low))
      val goalType = UnitType
      val example1 = new Example(
        preCxt = Map("x" -> LocValue(100), "y" -> LocValue(101)),
        preEnv = Map(100 -> NumValue(3), 101 -> NumValue(5)),
        postEnv = Map(100 -> NumValue(5), 101 -> NumValue(3)),
        resultValue = UnitValue
      )
      val example2 = new Example(
        preCxt = Map("x" -> LocValue(100), "y" -> LocValue(101)),
        preEnv = Map(100 -> NumValue(7), 101 -> NumValue(5)),
        postEnv = Map(100 -> NumValue(5), 101 -> NumValue(7)),
        resultValue = UnitValue
      )

      val constraint = new Constraint(
        inputType = inputType,
        goalType = goalType,
        examples = List(example1, example2)
      )

      val exp = new BidirectionalSynth(constraint).incrementalSearch
      println(exp.toString)
    }
  }

}
