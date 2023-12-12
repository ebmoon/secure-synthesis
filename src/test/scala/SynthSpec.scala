import org.scalatest.funspec.AnyFunSpec

class SynthSpec extends AnyFunSpec {

  import Eval._

  describe("Synth") {
    it("should synthesize correct swap function") {
      val inputType = Map("x" -> RefType(IntType(High), High), "y" -> RefType(IntType(High), High))
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
      assert(exp != None)

      // println(exp.getOrElse(BidirectionalLang.Unit).toString)
    }

    it("should fail to implement swap for different security labels") {
      val inputType = Map("x" -> RefType(IntType(Low), High), "y" -> RefType(IntType(High), High))
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

      val exp = new BidirectionalSynth(constraint, bound=20).incrementalSearch
      assert(exp == None)

      // println(exp.getOrElse(BidirectionalLang.Unit).toString)
    }

    it("should synthesize correct assignment (h := l)") {
      val inputType = Map("l" -> RefType(IntType(Low), High), "h" -> RefType(IntType(High), High))
      val goalType = UnitType
      val example1 = new Example(
        preCxt = Map("l" -> LocValue(100), "h" -> LocValue(101)),
        preEnv = Map(100 -> NumValue(3), 101 -> NumValue(5)),
        postEnv = Map(100 -> NumValue(3), 101 -> NumValue(3)),
        resultValue = UnitValue
      )
      val example2 = new Example(
        preCxt = Map("l" -> LocValue(100), "h" -> LocValue(101)),
        preEnv = Map(100 -> NumValue(7), 101 -> NumValue(1)),
        postEnv = Map(100 -> NumValue(7), 101 -> NumValue(7)),
        resultValue = UnitValue
      )

      val constraint = new Constraint(
        inputType = inputType,
        goalType = goalType,
        examples = List(example1, example2)
      )

      val exp = new BidirectionalSynth(constraint, bound = 20).incrementalSearch
      assert(exp != None)

      // println(exp.getOrElse(BidirectionalLang.Unit).toString)
    }

    it("should fail to implement basic assignment (l := h)") {
      val inputType = Map("l" -> RefType(IntType(Low), High), "h" -> RefType(IntType(High), High))
      val goalType = UnitType
      val example1 = new Example(
        preCxt = Map("l" -> LocValue(100), "h" -> LocValue(101)),
        preEnv = Map(100 -> NumValue(3), 101 -> NumValue(5)),
        postEnv = Map(100 -> NumValue(5), 101 -> NumValue(5)),
        resultValue = UnitValue
      )
      val example2 = new Example(
        preCxt = Map("l" -> LocValue(100), "h" -> LocValue(101)),
        preEnv = Map(100 -> NumValue(7), 101 -> NumValue(1)),
        postEnv = Map(100 -> NumValue(1), 101 -> NumValue(1)),
        resultValue = UnitValue
      )

      val constraint = new Constraint(
        inputType = inputType,
        goalType = goalType,
        examples = List(example1, example2)
      )

      val exp = new BidirectionalSynth(constraint, bound = 20).incrementalSearch
      assert(exp == None)

      // println(exp.getOrElse(BidirectionalLang.Unit).toString)
    }
  }

}
