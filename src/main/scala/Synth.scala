class Example(preCxt: Eval.Context, preEnv: Eval.Environment, postEnv: Eval.Environment, resultValue: Eval.Value)
class Constraint(goalType: Type, examples: List[Example])

object Synth {
  def bottomUpSynth(constraint: Constraint): Lang.Expression = {
    ???
  }
}
