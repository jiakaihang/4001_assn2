package ass02

import Proposition.Name

object Operations {
  
//  trait Ident extends Visitor[Boolean]{
//    def value = if (_) true else false
//  }
//  object Ident extends Ident
  
//  object tof extends (Proposition => Boolean) {
//    override def apply(p:Proposition) = Ident.visit(p)
//  }
  
  def eval(binding: Name => Boolean): Proposition => Boolean = {
//		  tof
		      error("TODO")
  }

  
  def evalV (binding: Name => Proposition) = {
	   (p: Proposition) => p match {
	     case False		=>false
	     case True		=>true
	     case Variable(_) => Variable(_)
	     case And(a,b)	=>a&b
	   }
  }

  def reduce: Proposition => Proposition = 
    error("TODO")

  def substitute(name: Name, truthVal: Boolean): Proposition => Proposition =
    error("TODO")

  def substituteAndReduce(name: Name, truthVal: Boolean) = 
    error("TODO")
  
  import scala.collection.SortedSet

  type Names = SortedSet[Name]
  val Names = SortedSet[Name] _
  def vars: Proposition => Names =
    error("TODO")
  
  def solve(goal: Proposition): Option[List[Name => Boolean]] =
    error("TODO")
  
  import OBDD._

  def obdd(names: List[Name])(p: Proposition): OBDD =
    error("TODO")
}