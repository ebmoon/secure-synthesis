class Example(preCxt: Eval.Context, preEnv: Eval.Environment, postEnv: Eval.Environment, resultValue: Eval.Value) {
  def correct(exp: Lang.Expression): Boolean = {
    val (env, value) = Eval.evaluate(preCxt, preEnv)(exp)
    env.equals(postEnv) && resultValue.equals(value)
  }
}

class Constraint(inputType: Map[String, Type], goalType: Type, examples: List[Example]) {
  def matchType(exp: Lang.Expression): Boolean = Typing.typeCheck(inputType, exp, goalType)
  def matchExample(exp: Lang.Expression): Boolean = examples.forall(_.correct(exp))
  def variables: List[String] = inputType.keys.toList
}

class Synth(constraint: Constraint) {
  type MutableMap = scala.collection.mutable.Map[Int, Lang.Expression]

  val variables = constraint.variables.map(Lang.Variable(_))
  val values = Lang.Unit :: Lang.Num(0) :: Lang.Num(1) :: variables
  val refExprs = values.map(Lang.Ref(_))
  val derefExprs = variables.map(Lang.Deref(_))
  val assignExprs =
    for {
      variable <- variables
      value <- values
    } yield Lang.Assign(variable, value)

  def bindExprs(expBins: Map[Int, List[Lang.Expression]])(n1: Int, n2: Int) =
    for {
      variable <- variables
      exp1 <- expBins.getOrElse(n1, List())
      exp2 <- expBins.getOrElse(n2, List())
    } yield Lang.Bind(variable, exp1, exp2)

  def iteExprs(expBins: Map[Int, List[Lang.Expression]])(n1: Int, n2:Int) =
    for {
      variable <- variables
      exp1 <- expBins.getOrElse(n1, List())
      exp2 <- expBins.getOrElse(n2, List())
    } yield Lang.Ite(variable, exp1, exp2)

  def bottomUpSearch: Lang.Expression = {
    var expBins = Map[Int, List[Lang.Expression]]()
    var n = 1;
    while (true) {
      val candidates = newTerms(n, expBins)
      candidates.foreach {
        exp => if (constraint.matchType(exp) && constraint.matchExample(exp)) return exp
      }
      expBins = expBins + (n -> candidates)
      n += 1
    }
    throw UnknownError  // Unreachable code
  }

  def newTerms(n: Int, expBins: Map[Int, List[Lang.Expression]]): List[Lang.Expression] = {
    if (n == 1) values
    else if (n == 2) refExprs ++ derefExprs
    else if (n == 3) {
      assignExprs ++
        (1 until n-2).flatMap(i => bindExprs(expBins)(i, n - i - 1)) ++
        (1 until n-2).flatMap(i => iteExprs(expBins)(i, n - i - 1))
    }
    else {
      (1 until n-2).flatMap(i => bindExprs(expBins)(i, n - i - 1)).toList ++
        (1 until n-2).flatMap(i => iteExprs(expBins)(i, n - i - 1))
    }
  }
}
