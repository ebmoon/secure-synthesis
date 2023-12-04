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

object Typing {
  type Context = Map[String, Type]

  def typeCheck(cxt: Context, exp: Lang.Expression, ty: Type): Boolean = {
    true  // Not implemented
  }
}