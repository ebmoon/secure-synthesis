class Example(preCxt: Eval.Context, preEnv: Eval.Environment, postEnv: Eval.Environment, resultValue: Eval.Value) {
  def correct(exp: Lang.Expression): Boolean = {
    val (env, value) = Eval.evaluate(preCxt, preEnv)(exp)
    env.equals(postEnv) && resultValue.equals(value)
  }
}

class Constraint(inputType: Map[String, Type], goalType: Type, examples: List[Example]) {
  def correct(exp: Lang.Expression): Boolean = {
    Typing.typeCheck(inputType, exp, goalType) && examples.forall(_.correct(exp))
  }
}

object Synth {
  def bottomUpSynth(constraint: Constraint): Lang.Expression = {
    ???
  }
}
