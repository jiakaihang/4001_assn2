package ass02

trait Visitor[R] {

  import Proposition.Name

  def value: Boolean => R
  def variable: Name => R
  def not: R => R
  def and: (R, R) => R
  def or: (R, R) => R
  def cond: (R, R, R) => R
  def equiv: (R, R) => R

  def visit(prop: Proposition): R = {
    prop match {
      case False               => value (false)
      case True                => value (true)
      case Variable(name)      => variable (name)
      case Not(p)              => not (visit (p))
      case And(p, q)           => and (visit (p), visit (q))
      case Or(p, q)            => or (visit (p), visit (q))
      case IfThenElse(p, q, r) => cond (visit (p), visit (q), visit (r))
      case Equivalent(p, q)    => equiv (visit (p), visit(q))
    }
  }
}

///////////////////////////////////////////////////////////////////////////////

// Sample of implementation of operations
// Identity can be used as a base class when implementing Visitor [Proposition]

trait Identity extends Visitor[Proposition] {
  def value = if (_) True else False
  def variable = Variable (_): Proposition	// cast to allow subtypes to return a non-Variable
  def not = ~_
  def and = _ & _
  def or = _ | _
  def cond = IfThenElse (_, _, _): Proposition
  def equiv = Equivalent (_, _): Proposition
}

object Identity extends Identity

// Sample of use of visit method to recursively copy structure of an Exp
object Copy extends (Proposition => Proposition) {
  override def apply(p: Proposition) = Identity.visit (p) // copies p
}
