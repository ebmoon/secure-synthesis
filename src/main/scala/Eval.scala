import java.io.InvalidObjectException

object Eval {
  type Location = Int

  sealed trait Value
  case class NumValue(n: Int) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case object UnitValue extends Value
  case class LocValue(l: Location) extends Value

  type Context = Map[String, Value]
  type Environment = Map[Location, Value]

  var counter: Location = 10000
  def freshLocation: Location = {
    val ret = counter
    counter = counter + 1
    ret
  }

  def evaluate(cxt: Context, env: Environment, e: Lang.Expression): (Environment, Value) = {
    e match {
      case Lang.Variable(id) => (env, cxt(id))
      case Lang.True => (env, BooleanValue(true))
      case Lang.False => (env, BooleanValue(false))
      case Lang.Num(n) => (env, NumValue(n))
      case Lang.Unit => (env, UnitValue)
      case Lang.Deref(sub) => {
        evaluate(cxt, env, sub) match {
          case (_, LocValue(l)) => (env, env(l))  // sub is value without side effect
          case _ => throw new InvalidObjectException("Dereference from non-location")
        }
      }
      case Lang.Assign(e1, e2) => {
        // e1 and e2 are values without side effect
        val (_, lvalue) = evaluate(cxt, env, e1)
        val (_, rvalue) = evaluate(cxt, env, e2)
        lvalue match {
          case LocValue(l) => (env + (l -> rvalue), UnitValue)
          case _ => throw new InvalidObjectException("Update to non-location")
        }
      }
      case Lang.Ref(sub) => {
        // sub is value without side effect
        val (_, value) = evaluate(cxt, env, sub)
        val l = freshLocation
        (env + (l -> value), LocValue(l))
      }
      case Lang.Bind(x, e1, e2) => {
        val (env1, value1) = evaluate(cxt, env, e1)
        val cxt1 = cxt + (x.id -> value1)
        evaluate(cxt1, env1, e2)
      }
      case Lang.Ite(cond, e1, e2) => {
        // cond is value without side effect
        val (_, condValue) = evaluate(cxt, env, cond)
        condValue match {
          case BooleanValue(true) => evaluate(cxt, env, e1)
          case BooleanValue(false) => evaluate(cxt, env, e2)
          case _ => throw new InvalidObjectException("Non-Boolean condition value")
        }
      }
    }
  }

  def evaluate(cxt: Context, env: Environment, e: BidirectionalLang.Expression): (Environment, Value) = {
    e match {
      case BidirectionalLang.Variable(id) => (env, cxt(id))
      case BidirectionalLang.True => (env, BooleanValue(true))
      case BidirectionalLang.False => (env, BooleanValue(false))
      case BidirectionalLang.Num(n) => (env, NumValue(n))
      case BidirectionalLang.Unit => (env, UnitValue)
      case BidirectionalLang.Deref(sub) => {
        evaluate(cxt, env, sub) match {
          case (_, LocValue(l)) => (env, env(l))  // sub is value without side effect
          case _ => throw new InvalidObjectException("Dereference from non-location")
        }
      }
      case BidirectionalLang.Assign(e1, e2) => {
        // e1 and e2 are values without side effect
        val (_, lvalue) = evaluate(cxt, env, e1)
        val (_, rvalue) = evaluate(cxt, env, e2)
        lvalue match {
          case LocValue(l) => (env + (l -> rvalue), UnitValue)
          case _ => throw new InvalidObjectException("Update to non-location")
        }
      }
      case BidirectionalLang.Ref(sub) => {
        // sub is value without side effect
        val (_, value) = evaluate(cxt, env, sub)
        val l = freshLocation
        (env + (l -> value), LocValue(l))
      }
      case BidirectionalLang.Bind(x, e1, e2) => {
        val (env1, value1) = evaluate(cxt, env, e1)
        val cxt1 = cxt + (x.id -> value1)
        evaluate(cxt1, env1, e2)
      }
      case BidirectionalLang.Ite(cond, e1, e2) => {
        // cond is value without side effect
        val (_, condValue) = evaluate(cxt, env, cond)
        condValue match {
          case BooleanValue(true) => evaluate(cxt, env, e1)
          case BooleanValue(false) => evaluate(cxt, env, e2)
          case _ => throw new InvalidObjectException("Non-Boolean condition value")
        }
      }
      case BidirectionalLang.ElimToIntro(sub) => evaluate(cxt, env, sub)
      case BidirectionalLang.ElimValueToIntroValue(sub) => evaluate(cxt, env, sub)
    }
  }
}
