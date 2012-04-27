package ass02

import Proposition.Name

object Operations {
  
//  trait prop2bool extends Visitor[Boolean]{
//    def value = if(_) true else false
//    def variable = binding(q)
//    def not = !_
//    def and = _&&_
//    def or	= _||_
//    def cond = if(_) _ else _
//    def equiv = _==_
//  }
//  
//  object prop2bool extends prop2bool
  
  
  def eval(binding: Name => Boolean): Proposition => Boolean = //{(p: Proposition) => prop2bool.visit(p)}
  {
		  (p: Proposition) => p match{
		    case False				=> false
		    case True				=> true
		    case Variable(q)		=> binding(q)
		    case And(r, s)			=> eval(binding)(r)&&eval(binding)(s)
		    case Or(r, s)			=> eval(binding)(r)||eval(binding)(s)
		    case Not(q)				=> !eval(binding)(q)
		    case IfThenElse(r,s,t)	=> if(eval(binding)(r)) eval(binding)(s) else eval(binding)(t)
		    case Equivalent(r, s)	=> eval(binding)(r)==eval(binding)(s)
		  }
  }
  def prt(p: Proposition, r: Proposition, s: Proposition) = {println("\n{p = "+p+"\tr = "+r+"\ts = "+s+"}");}
  def reduce: Proposition => Proposition = 
  {
    (p: Proposition) => p match{
	  case False				=> False
	  case True					=> True
	  case Variable(q)			=> Variable(q)
	  case Not(q)				=> if(q==True) False 
	      							else if(q==False) True
	      							else ~reduce(q)
	  case And(r, s)			=> 	if(r==False || s==False) False
	  								else if(r==True){
	  								  if(s==True)True
	  								  else if(s.isInstanceOf[Variable]) s
	  								  else reduce(s)
	  								}
	  								else if(r.isInstanceOf[Variable]){
	  								  if(s==True) r
	  								  else if(s.isInstanceOf[Variable]) r&s
	  								  else reduce(r&reduce(s))
	  								}
	  								else{
	  								  if(s==True) reduce(r)
	  								  else if(s.isInstanceOf[Variable]) reduce(reduce(r)&s)
	  								  else reduce(reduce(r)&reduce(s))
	  								}
	  								
//	    							if(r==False || s==False) False
//	  	      						else if(r!=True && s==True) reduce(r)
//	      							else if(r==True && s!=True) reduce(s)
//	      							else if(r!=True && s!=True) reduce(r)&reduce(s)
//	      							else True
	  case Or(r, s)				=> 	if(r==True || s==True) True
	  								else if(r==False){
	  								  if(s==False)False
	  								  else if(s.isInstanceOf[Variable]) s
	  								  else reduce(s)
	  								}
	  								else if(r.isInstanceOf[Variable]){
	  								  if(s==False) r
	  								  else if(s.isInstanceOf[Variable]) r|s
	  								  else reduce(r|reduce(s))
	  								}
	  								else{
	  								  if(s==False) reduce(r)
	  								  else if(s.isInstanceOf[Variable]) reduce(reduce(r)|s)
	  								  else reduce(reduce(r)|reduce(s))
	  								}
//	    							if(r==True || s==True) True
//	  	      						else if(r!=False && s==False) reduce(r)
//	      							else if(r==False && s!=False) reduce(s)
//	      							else if(r!=False && s!=False) reduce(r)|reduce(s)
//	      							else False
      case IfThenElse(r, s, t)	=> if(r==True) reduce(s)
      								else if(r==False) reduce(t)
      								else IfThenElse(reduce(r),reduce(s),reduce(t))
      case Equivalent(r, s)		=> if((r==True && s==True)||(r==False && s==False)) True
      								else if(r!=True && r!=False && s==True) reduce(r)
      								else if(r==True && s!=True && s!=False) reduce(s)
      								else if(r!=True && r!=False && s==False) reduce(~r)
      								else if(r==False && s!=True && s!=False) reduce(~s)
      								else if(r!=True && r!=False && s!=True && s!=False) reduce(r)<->reduce(s)
      								else False
    }
  }

  def substitute(name: Name, truthVal: Boolean): Proposition => Proposition =
  {
    (p: Proposition) => p match{
      case False 				=> False
      case True					=> True
      case Variable(q)			=> if(q==name && truthVal==true) True
      								else if(q==name && truthVal==false) False
      								else Variable(q)
      case Not(q)				=> ~substitute(name, truthVal)(q)
      case And(r, s)			=> substitute(name,truthVal)(r) & substitute(name,truthVal)(s)
      case Or(r, s)				=> substitute(name,truthVal)(r) | substitute(name,truthVal)(s)
      case IfThenElse(r, s, t)	=> IfThenElse(substitute(name,truthVal)(r),substitute(name,truthVal)(s),substitute(name,truthVal)(t))
      case Equivalent(r, s)		=> substitute(name,truthVal)(r) <-> substitute(name,truthVal)(s)
    }
  }

  def substituteAndReduce(name: Name, truthVal: Boolean) = substitute(name,truthVal) andThen reduce

  
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