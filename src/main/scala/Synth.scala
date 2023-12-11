class Example(preCxt: Eval.Context, preEnv: Eval.Environment, postEnv: Eval.Environment, resultValue: Eval.Value) {
  def matchExample(exp: Lang.Expression): Boolean = {
    val (env, value) = Eval.evaluate(preCxt, preEnv, exp)
    env.equals(postEnv) && resultValue.equals(value)
  }
  def matchExample(exp: BidirectionalLang.Expression): Boolean = {
    val (env, value) = Eval.evaluate(preCxt, preEnv, exp)
    env.equals(postEnv) && resultValue.equals(value)
  }
}

class Constraint(val inputType: Map[String, Type], goalType: Type, examples: List[Example]) {
  def matchType(exp: Lang.Expression): Boolean = StandardTyping.typeCheck(inputType, exp, goalType)
  def matchType(exp: BidirectionalLang.Expression): Boolean = BidirectionalTyping.typeCheck(inputType, exp, goalType)
  def matchExample(exp: Lang.Expression): Boolean = examples.forall(_.matchExample(exp))
  def matchExample(exp: BidirectionalLang.Expression): Boolean = examples.forall(_.matchExample(exp))
  def variables: List[String] = inputType.keys.toList
}

// Currently has an error that bind variables are never used
class Synth(constraint: Constraint) {
  val variables = constraint.variables.map(Lang.Variable(_))
  val values = Lang.Unit :: Lang.True :: Lang.False :: Lang.Num(0) :: Lang.Num(1) :: variables
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
    throw new UnknownError  // Unreachable code
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

class BidirectionalSynth(constraint: Constraint, bound: Int = -1) {

  import BidirectionalLang._

  type Context = Map[Variable, Type]

  var counter: Int = 0
  def freshVar: Variable = {
    val id = s"x${counter}"
    counter = counter + 1
    Variable(id)
  }

  def elimValues(variables: Context): List[(ElimValue, Type)] = {
    (Unit, UnitType) ::
      (Num(0), IntType(SecurityLabel.freshVar)) ::
      (Num(1), IntType(SecurityLabel.freshVar)) ::
      variables.toList
  }

  def introValues(variables: Context): List[(IntroValue, Type)] =
    (True, BooleanType(SecurityLabel.freshVar)) ::
      (False, BooleanType(SecurityLabel.freshVar)) ::
      elimValues(variables).map { case (exp, ty) => (ElimValueToIntroValue(exp), ty) }

  def refExprs(variables: Context): List[(Ref, Type)] =
    introValues(variables).map{ case (exp, ty) => (Ref(exp), ty) }

  def derefExprs(variables: Context): List[(Deref, Type)] = {
    for {
      (exp, ty) <- elimValues(variables)
      innerType <- ty match {
        case RefType(inner, _) => Some(inner)
        case _ => None
      }
    } yield (Deref(exp), innerType)
  }

  def validAssign(ty1: Type, ty2: Type): Boolean =
    ty1 match {
      case RefType(ty, l) => BidirectionalTyping.sub(ty2, ty) && BidirectionalTyping.guard(l, ty)
      case _ => false
    }

  def assignExprs(variables: Context): List[(Assign, Type)] = {
    for {
      (variable, variableType) <- variables.toList
      (value, valueType) <- introValues(variables)
      if validAssign(variableType, valueType)
    } yield (Assign(variable, value), UnitType)
  }

  def iteExprs(variables: Context)(n1: Int, n2:Int): List[(Ite, Type)] =
    for {
      (variable, ty) <- elimValues(variables).filter {
        case (_, BooleanType(_)) => true
        case _ => false
      }
      (exp1, ty1) <- newIntroTerms(n1, variables)
      (exp2, ty2) <- newIntroTerms(n2, variables)
      if BidirectionalTyping.sub(ty1, ty2) || BidirectionalTyping.sub(ty2, ty1)
      ty_join = BidirectionalTyping.join(ty1, ty2)
      if ty_join.isDefined
    } yield (Ite(variable, exp1, exp2), ty_join.getOrElse(UnitType))

  def bindExprs(variables: Context)(n1: Int, n2: Int): List[(Bind, Type)] =
    for {
      (exp1, ty1) <- newElimTerms(n1, variables)
      variable = freshVar
      (exp2, ty2) <- newTerms(n2, variables + (variable -> ty1))
    } yield (Bind(variable, exp1, exp2), ty2)

  def incrementalSearch: Option[Expression] = {
    val variables = constraint.variables.map(x => (Variable(x), constraint.inputType(x))).toMap
    var n = 1

    while (n != bound) {
      val candidates = newTerms(n, variables)
      val wellTypedTerms = candidates.filter{ case (exp, _) => constraint.matchType(exp) }

      wellTypedTerms.foreach { case (exp, _) =>
        if (constraint.matchExample(exp)) return Some(exp)
        else ()
      }
      n += 1
    }
    None
  }

  def newTerms(n: Int, variables: Context): List[(Expression, Type)] = {
    newIntroTerms(n, variables) ++
      (2 until 4).filter(i => n-i-2 >= 1).flatMap{ i => bindExprs(variables)(i, n-i-2) }
    // Not 1 until 4 because we don't want bind expression like bind x = x in ...
  }

  def newElimTerms(n: Int, variables: Context): List[(ElimExpression, Type)] = {
    if (n == 1) elimValues(variables)
    else if (n == 2) derefExprs(variables)
    else if (n == 3) assignExprs(variables)
    else List[(ElimExpression, Type)]()
  }

  def newIntroTerms(n: Int, variables: Context): List[(IntroExpression, Type)] = {
    if (n < 1) List[(IntroExpression, Type)]()
    else if (n == 1) introValues(variables)
    else if (n == 2)
      derefExprs(variables).map { case (exp, ty) => (ElimToIntro(exp), ty) } ++
        refExprs(variables)
    else if (n == 3)
      assignExprs(variables).map { case (exp, ty) => (ElimToIntro(exp), ty) } ++
        iteExprs(variables)(1, 1)
    else
      (1 until n-2).flatMap(i => iteExprs(variables)(i, n - i - 1)).toList
  }
}
