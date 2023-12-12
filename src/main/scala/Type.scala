
sealed trait SecurityLabel {
  def toString: String
  def join(other: SecurityLabel): SecurityLabel
  // Returns overapproximation
  def <=(other: SecurityLabel): Boolean
}
case object Low extends SecurityLabel {
  override def toString: String = "L"
  override def join(other: SecurityLabel): SecurityLabel = other
  override def <=(other: SecurityLabel): Boolean = true
}
case object High extends SecurityLabel {
  override def toString: String = "H"
  override def join(other: SecurityLabel): SecurityLabel = this
  override def <=(other: SecurityLabel): Boolean = {
    other match {
      case Low => false
      case _ => true
    }
  }
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
  override def <=(other: SecurityLabel): Boolean = true
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
  override def <=(other: SecurityLabel): Boolean = (l1 <= other) && (l2 <= other)
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

object StandardTyping {
  type Context = Map[String, Type]

  // Return the type of expression if well-typed, return None if ill-typed
  def inferType(cxt: Context, exp: Lang.Expression): Option[Type] = {
    None
  }

  def typeCheck(cxt: Context, exp: Lang.Expression, ty: Type): Boolean = {
    val lCheck: Option[SecurityLabel] = labelCheck(cxt, exp);
    lCheck.isDefined
  }

  private def labelCheck(cxt: Context, exp:Lang.Expression): Option[SecurityLabel] = {
    exp match {
      /* leaf terms */
      case Lang.Variable(id) => {
        cxt.get(id) match {
          case Some(IntType(l)) => Option(l)
          case Some(BooleanType(l)) => Option(l)
          case Some(RefType(_, l)) => Option(l)
          case None => throw new UnknownError
          case _ => throw new IllegalArgumentException
        };
      }
      case Lang.True => Option(Low)
      case Lang.False => Option(Low)
      case Lang.Num(_) => Option(Low)
      case Lang.Unit => Option(Low)

      /* recursive terms */
      case Lang.Deref(e) => labelCheck(cxt, e)
      case Lang.Ref(e) => labelCheck(cxt, e)
      case Lang.Assign(e1, e2) => { //only place can fail type checks
        val e1Check = labelCheck(cxt, e1);
        val e2Check = labelCheck(cxt, e2);

        if (e1Check.isEmpty || e2Check.isEmpty)
          None // type error in subtree
        else if (e1Check.get == Low && e2Check.get == High)
          None // violates information flow
        else e1Check // return pc of assigned var
      }
      case Lang.Bind(x, e1, e2) => {
        //val condCheck = labelCheck(cxt, x); // unknown, and unneeded
        val e1Check = labelCheck(cxt, e1);
        val e2Check = labelCheck(cxt, e2);

        if (e1Check.isEmpty || e2Check.isEmpty)
          None // type error in subtree
        else Option(e1Check.get.join(e2Check.get))
      }
      case Lang.Ite(cond, e1, e2) => {
        val condCheck = labelCheck(cxt, cond);
        val e1Check = labelCheck(cxt, e1);
        val e2Check = labelCheck(cxt, e2);

        if (condCheck.isEmpty || e1Check.isEmpty || e2Check.isEmpty)
          None // type error in subtree
        else if (condCheck.get == High && (e1Check.get == Low || e2Check.get == Low))
          None // violates implicit IFC
        else condCheck // return label of conditional guard
      }
    }
  }
}

object BidirectionalTyping {
  import BidirectionalLang._

  type Context = Map[String, Type]

  def guard(l: SecurityLabel, ty: Type): Boolean = {
    ty match {
      case UnitType => true
      case IntType(l_ty) => l <= l_ty
      case BooleanType(l_ty) => l <= l_ty
      case RefType(_, l_ty) => l <= l_ty
    }
  }

  def sub(ty1: Type, ty2: Type): Boolean = {
    (ty1, ty2) match {
      case (UnitType, UnitType) => true
      case (IntType(l1), IntType(l2)) => l1 <= l2
      case (BooleanType(l1), BooleanType(l2)) => l1 <= l2
      case (RefType(tty1, l1), RefType(tty2, l2)) => sub(tty1, tty2) && sub(tty2, tty1) && l1 <= l2
      case (_, _) => false
    }
  }

  def join(ty1: Type, ty2: Type): Option[Type] = {
    (ty1, ty2) match {
      case (UnitType, UnitType) => Some(UnitType)
      case (IntType(l1), IntType(l2)) => Some(IntType(l1.join(l2)))
      case (BooleanType(l1), BooleanType(l2)) => Some(BooleanType(l1.join(l2)))
      case (RefType(tty1, l1), RefType(tty2, l2)) =>
        if (sub(tty1, tty2) && sub(tty2, tty1)) Some(RefType(tty1, l1.join(l2)))
        else None
      case (_, _) => None
    }
  }

  // Return the type of expression if well-typed, return None if ill-typed
  def inferType(cxt: Context, exp: Expression): Option[Type] = {
    None
  }

  def typeCheck(cxt: Context, exp: Expression, ty: Type): Boolean = {
    true  // Not implemented
  }
}