package ass02

import scala.util.parsing.combinator._
import scala.NotNull

trait PropRecognizer extends RegexParsers {

  val ident = """[a-zA-Z]\w*""".r

  val biArrow = "<=>" | "<+>"
  val rightArrow = "=>"
  val leftArrow = "<="
  val disjOp = "|"
  val conjOp = "&"
  val negOp = "~"

  lazy val prop: Parser[Proposition] = equiv
  
  lazy val pos: Parser[Proposition] = ident^^{Variable(_)}|"("~>prop<~")"
  
  lazy val negs: Parser[Proposition] = negOp~neg ^^{case negOp~neg=>Not(neg)}
  
  lazy val neg: Parser[Proposition] = negs | pos
  
  lazy val conj: Parser[Proposition] = rep1sep(neg, conjOp)^^{case list=>{//println("conj list = "+list)
      if(list.length==1) list(0) else list.reduceLeft(_.&(_))}}
  
  lazy val disj: Parser[Proposition] = rep1sep(conj,disjOp)^^{case list=>{//println("disj list = "+list)
	  if(list.length==1) list(0) else list.reduceLeft(_.|(_))}}
  
  lazy val impls: Parser[Proposition] = rep1sep(disj,rightArrow)^^{case list=>{//println("rightArrow list = "+list)
	  if(list.length==1) list(0) else list.reduceRight((_).unary_~.|(_))}} |
	  rep1sep(disj,leftArrow)^^{case list=>{									//println("leftArrow list = "+list)
	  if(list.length==1) list(0) else list.reduceLeft((_).unary_~.|(_))}}
	 
  lazy val impl: Parser[Proposition] = impls
  
  lazy val equiv: Parser[Proposition] = impl~opt(biArrow~impl)^^{
  	case p~Some("<=>"~q)	=> (p.&(q)).|(p.unary_~.&(q.unary_~))
  	case p~Some("<+>"~q)	=>(p.&(q.unary_~)).|((p.unary_~).&(q))
  	case p~None				=> p}
}