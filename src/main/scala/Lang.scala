sealed trait SecurityLabel
case object Low extends SecurityLabel
case object High extends SecurityLabel

sealed trait Type
case object UnitType extends Type
case class IntType(l: SecurityLabel) extends Type
case class RefType(ty: Type, l: SecurityLabel) extends Type

class Lang {
  sealed trait Expression
  case class Variable(id: String) extends Expression
  case class Num(n: Int) extends Expression
  case object Unit extends Expression
  case class Deref(e: Expression) extends Expression
  case class Assign(e1: Expression, e2: Expression) extends Expression
  case class Ref(e: Expression) extends Expression
  case class Bind(v: Variable, e1: Expression, e2: Expression) extends Expression
  case class Ite(cond: Expression, e1: Expression, e2: Expression) extends Expression
}

// Language for bidirectional type checking (for type-guided synthesis)
// Not used in naive enumerative synthesizer
class BidirectionalLang {
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