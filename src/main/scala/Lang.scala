object Lang {
  sealed trait Expression {
    def toString: String
  }

  sealed trait Value extends Expression

  case class Variable(id: String) extends Value {
    override val toString: String = id
  }
  case object True extends Value {
    override val toString: String = "true"
  }
  case object False extends Value {
    override val toString: String = "false"
  }
  case class Num(n: Int) extends Value {
    override val toString: String = n.toString
  }
  case object Unit extends Value {
    override val toString: String = "()"
  }
  case class Deref(e: Variable) extends Expression {
    override def toString: String = s"!${e.toString}"
  }
  case class Assign(e1: Variable, e2: Value) extends Expression {
    override def toString: String = s"${e1.toString} := ${e2.toString}"
  }
  case class Ref(e: Value) extends Expression {
    override def toString: String = s"ref ${e.toString}"
  }
  case class Bind(x: Variable, e1: Expression, e2: Expression) extends Expression {
    override def toString: String =
      s"""bind ${x.toString} = ${e1.toString} in
         |${e2.toString}
         |""".stripMargin
  }
  case class Ite(cond: Value, e1: Expression, e2: Expression) extends Expression {
    override def toString: String =
      s"if ${cond.toString} then ${e1.toString} else ${e2.toString}"
  }
}

object BidirectionalLang {
  sealed trait Expression {
    def toString: String
    def size: Int
  }

  sealed trait ElimExpression extends Expression
  case class Deref(e: ElimValue) extends ElimExpression {
    override def toString: String = s"!${e.toString}"
    override val size: Int = 2
  }
  case class Assign(e1: Variable, e2: IntroValue) extends ElimExpression {
    override def toString: String = s"${e1.toString} := ${e2.toString}"
    override val size: Int = 3
  }

  // Not a value in the manner of evaluation, used for value restriction
  sealed trait ElimValue extends ElimExpression
  case class Variable(id: String) extends ElimValue {
    override val toString: String = id
    override val size: Int = 1
  }
  case class Num(n: Int) extends ElimValue {
    override val toString: String = n.toString
    override val size: Int = 1
  }
  case object Unit extends ElimValue {
    override val toString: String = "()"
    override val size: Int = 1
  }

  sealed trait IntroExpression extends Expression
  case class ElimToIntro(e: ElimExpression) extends IntroExpression {
    override def toString: String = e.toString
    override val size: Int = e.size
  }
  case class Ref(e: IntroValue) extends IntroExpression {
    override def toString: String = s"ref ${e.toString}"
    override val size: Int = 2
  }
  case class Ite(cond: ElimValue, e1: IntroExpression, e2: IntroExpression) extends IntroExpression {
    override def toString: String =
      s"if ${cond.toString} then ${e1.toString} else ${e2.toString}"
    override val size: Int = 2 + e1.size + e2.size
  }

  sealed trait IntroValue extends IntroExpression
  case object True extends IntroValue {
    override val toString: String = "true"
    override val size: Int = 1
  }
  case object False extends IntroValue {
    override val toString: String = "false"
    override val size: Int = 1
  }
  case class ElimValueToIntroValue(e: ElimValue) extends IntroValue {
    override def toString: String = e.toString
    override val size: Int = e.size
  }

  case class Bind(x: Variable, e1: ElimExpression, e2: Expression) extends Expression {
    override def toString: String = s"bind ${x.toString} = ${e1.toString} in ${e2.toString}"
    override def size: Int = 2 + e1.size + e2.size
  }
}