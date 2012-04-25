package ass02

import Proposition.Name

/** An ordered binary decision diagram.
 *  Provides a graph-based implementation for efficient representation of boolean propositions.
 *  @author potter
 */
trait OBDD {
  /** The negation of an OBDD is easily calculated.
   *  For efficiency, nodes should usually cache the negation.
   */
  def negate: OBDD
}

/** The OBDD object provides a scope for implementations of OBDD nodes.
 *  Non-trivial nodes have private constructors.
 *  Factory methods for nodes operate through a cache of nodes, ensuring that
 *  the same OBDD structures are shared rather than duplicated.
 */
object OBDD {
  object FalseOBDD extends OBDD { def negate = TrueOBDD }
  object TrueOBDD extends OBDD { def negate = FalseOBDD }

  class PositiveLiteral private[OBDD] (val name: Name)
      extends OBDD {
    lazy val negate = negativeLiteral (name)
    override val hashCode: Int = name.hashCode
    override def equals(other: Any) = other match {
      case that: PositiveLiteral => name == that.name
      case _                     => false
    }
  }

  class NegativeLiteral private[OBDD] (val name: Name)
      extends OBDD {
    lazy val negate = positiveLiteral (name)
    override val hashCode: Int = name.hashCode
    override def equals(other: Any) = other match {
      case that: NegativeLiteral => name == that.name
      case _                     => false
    }
  }

  class Conditional private[OBDD] (val name: Name, val ifTrue: OBDD, val ifFalse: OBDD)
      extends OBDD {
    lazy val negate = conditional (name, ifTrue.negate, ifFalse.negate)
    override val hashCode: Int = name.hashCode ^ ifTrue.hashCode ^ ifFalse.hashCode
    override def equals(other: Any) = other match {
      case that: Conditional =>
        name == that.name &&
          ifTrue.eq (that.ifTrue) &&
          ifFalse.eq (that.ifFalse)
      case _ => false
    }
  }

  class Definition private[OBDD] (val name: Name, val obdd: OBDD)
      extends OBDD {
    lazy val negate = definition (name, obdd.negate)
    override lazy val hashCode: Int = name.hashCode ^ obdd.hashCode
    override def equals(other: Any) = other match {
      case that: Definition =>
        name == that.name &&
          obdd.eq (that.obdd)
      case _ => false
    }
  }
  import scala.collection.mutable.Map // default Map is immutable

  val cache: Map[OBDD, OBDD] = Map ()

  /** get existing node from cache if it exists; otherwise add the node to the cache. */
  def apply(obdd: OBDD): OBDD = cache.getOrElseUpdate (obdd, obdd)

  // following factory methods use OBDD.apply to check for cached values

  def positiveLiteral(name: Name) = OBDD (new PositiveLiteral(name))
  def negativeLiteral(name: Name) = OBDD (new NegativeLiteral(name))

  def conditional(name: Name, ifTrue: OBDD, ifFalse: OBDD): OBDD =
    OBDD (new Conditional(name, ifTrue, ifFalse))

  def definition(name: Name, obdd: OBDD) =
    OBDD (new Definition(name, obdd))

  import Proposition._

  /** convert from OBDD.Node to Proposition */
  implicit def obdd2Exp(obdd: OBDD): Proposition = obdd match {
    case FalseOBDD               => False
    case TrueOBDD                => True

    // following cases rely on implicit conversions with name2Var
    case posLit: PositiveLiteral => posLit.name
    case negLit: NegativeLiteral => ~negLit.name

    // following cases rely on more implicit conversions with obdd2Exp (applied recursively!)
    case cond: Conditional       => IfThenElse (cond.name, cond.ifTrue, cond.ifFalse)
    case defn: Definition        => Equivalent (defn.name, defn.obdd)
  }
}