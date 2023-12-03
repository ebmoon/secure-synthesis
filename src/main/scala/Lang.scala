sealed trait SecurityLabel {
  def toString: String
}
case object Low extends SecurityLabel {
  override def toString: String = "L"
}
case object High extends SecurityLabel {
  override def toString: String = "H"
}

sealed trait Type {
  def toString: String
}
case object UnitType extends Type {
  override def toString: String = "Unit"
}
case class IntType(l: SecurityLabel) extends Type {
  override def toString: String = s"Int(${l.toString})"
}
case class RefType(ty: Type, l: SecurityLabel) extends Type {
  override def toString: String = s"${ty.toString} ref(${l.toString})"
}

object Lang {
  sealed trait Expression {
    def toString: String
    def size: Int
  }

  sealed trait Value extends Expression

  case class Variable(id: String) extends Value {
    override val toString: String = id
    override val size = 1
  }
  case class Num(n: Int) extends Value {
    override val toString: String = n.toString
    override val size = 1
  }
  case object Unit extends Value {
    override val toString: String = "()"
    override val size = 1
  }
  case class Deref(e: Value) extends Expression {
    override def toString: String = s"!${e.toString}"
    override val size = e.size + 1
  }
  case class Assign(e1: Value, e2: Value) extends Expression {
    override def toString: String = s"${e1.toString} := ${e2.toString}"
    override val size = e1.size + e2.size + 1
  }
  case class Ref(e: Value) extends Expression {
    override def toString: String = s"ref ${e.toString}"
    override val size = e.size + 1
  }
  case class Bind(x: Variable, e1: Expression, e2: Expression) extends Expression {
    override def toString: String =
      s"""bind ${x.toString} = ${e1.toString} in
         |${e2.toString}
         |""".stripMargin
    override val size = e1.size + e2.size + 1
  }
  case class Ite(cond: Value, e1: Expression, e2: Expression) extends Expression {
    override def toString: String =
      s"if ${cond.toString} then ${e1.toString} else ${e2.toString}"
    override val size = cond.size + e1.size + e2.size + 1
  }
}

// Language for bidirectional type checking (for type-guided synthesis)
// Not used in naive enumerative synthesizer
object BidirectionalLang {
  sealed trait ElimExpression
  case class Variable(id: String) extends ElimExpression
  case class Num(n: Int) extends ElimExpression
  case object Unit extends ElimExpression
  case class Deref(e: ElimExpression) extends ElimExpression
  case class Assign(e1: ElimExpression, e2: IntroExpression) extends ElimExpression

  sealed trait IntroExpression
  case class ElimToIntro(e: ElimExpression) extends IntroExpression
  case class Ref(e: IntroExpression) extends IntroExpression
  case class Bind(v: Variable, e1: ElimExpression, e2: IntroExpression) extends IntroExpression
  case class Ite(cond: ElimExpression, e1: IntroExpression, e2: IntroExpression) extends IntroExpression
}