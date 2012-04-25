package ass02

import Proposition.Name // import allows type Name to be used without qualification

sealed trait Proposition { p => // alias for this  
  // operators for constructing compound propositions
  def unary_~ : Proposition = Not (p)
  def &(q: Proposition): Proposition = And (p, q)
  def |(q: Proposition): Proposition = Or (p, q)
  def <->(q: Proposition): Proposition = Equivalent (p, q)
} // end of trait Proposition

object Proposition {
  type Name = String //  identifier for propositional variables
  implicit def name2Var(name: Name) = Variable (name)
}

// subclasses of Proposition follow

object False extends Proposition { override def toString = "0" }
object True extends Proposition { override def toString = "1" }

case class Variable(name: Name) extends Proposition with Ordered[Variable] {
  override def toString = name
  override def compare(that: Variable): Int = name compare (that.name)
}

case class Not(p: Proposition) extends Proposition {
  override def toString = "~" + p
}

abstract class Prop2(op: String, p: Proposition, q: Proposition) extends Proposition {
  override def toString = "(" + p + op + q + ")"
}

case class And(p: Proposition, q: Proposition) extends Prop2("&", p, q)
case class Or(p: Proposition, q: Proposition) extends Prop2("|", p, q)

case class Equivalent(p: Proposition, q: Proposition) extends Prop2("<=>", p, q)

case class IfThenElse(p: Proposition, q: Proposition, r: Proposition) extends Proposition {
  override def toString = "(" + p + "?" + q + ":" + r + ")"
}

