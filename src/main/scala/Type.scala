sealed trait SecurityLabel {
  def toString: String
  def join(other: SecurityLabel): SecurityLabel
}
case object Low extends SecurityLabel {
  override def toString: String = "L"
  override def join(other: SecurityLabel): SecurityLabel = other
}
case object High extends SecurityLabel {
  override def toString: String = "H"
  override def join(other: SecurityLabel): SecurityLabel = this
}
// Temporary security labels used for type inference
case class SecurityVar(id: String) extends SecurityLabel {
  override def toString: String = id
  override def join(other: SecurityLabel): SecurityLabel = {
    other match {
      case Low => this
      case High => other
      case _ => Join(this, other)
    }
  }
}
case class Join(l1: SecurityLabel, l2: SecurityLabel) extends SecurityLabel {
  override def toString: String = s"${l1.toString} |_| ${l2.toString}"
  override def join(other: SecurityLabel): SecurityLabel = {
    other match {
      case Low => this
      case High => other
      case _ => Join(this, other)
    }
  }
}

object SecurityLabel {
  var counter: Int = 0
  def freshVar: SecurityVar = {
    val id = s"l${counter}"
    counter = counter + 1
    SecurityVar(id)
  }
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
case class BooleanType(l: SecurityLabel) extends Type {
  override def toString: String = s"Boolean(${l.toString})"
}
case class RefType(ty: Type, l: SecurityLabel) extends Type {
  override def toString: String = s"${ty.toString} ref(${l.toString})"
}

object Typing {
  type Context = Map[String, Type]

  def typeCheck(cxt: Context, exp: Lang.Expression, ty: Type): Boolean = {
    true  // Not implemented
  }
}