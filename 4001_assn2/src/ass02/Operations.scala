package ass02

import Proposition.Name

object Operations {
  
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
  
  trait reducer extends Identity{
    override def not = {(p:Proposition) => 
      					if(p==False) True else if(p==True) False else ~p}
    override def and = {(p:Proposition, q:Proposition) => 	
      					if(p==False||q==False) False
    					else if(p==True&&q==True) True
    					else if(p==True) q
    					else if(q==True) p
    					else p&q}
    override def or = {(p:Proposition, q:Proposition) =>	
      					if(p==True||q==True) True
    					else if(p==False&&q==False) False
    					else if(p==False) q
    					else if(q==False) p
    					else p|q}
    override def cond = {(p:Proposition, q:Proposition, r:Proposition) =>
      					if(p==True) q else if(p==False) r else IfThenElse(p,q,r)}
    override def equiv= {(p:Proposition, q:Proposition) => 
      					if((p==True && q==True)||(p==False && q==False)) True
      					else if((p==True && q==False)||(p==False && q==True)) False
      					else if(p==True) q
      					else if(p==False) ~q
      					else if(q==True) p
      					else if(q==False) ~p
      					else p<->q}
  }
  object reducer extends reducer
  def reduce: Proposition => Proposition = reducer.visit(_)


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
  def vars: Proposition => Names ={
    (p: Proposition) => p match{
      		case True				=> Names()
      		case False				=> Names()
		    case Variable(q)		=> Names(q)
		    case And(r, s)			=> vars(r)++vars(s)
		    case Or(r, s)			=> vars(r)++vars(s)
		    case Not(q)				=> vars(q)
		    case IfThenElse(r,s,t)	=> vars(r)++vars(s)++vars(t)
		    case Equivalent(r, s)	=> vars(r)++vars(s)
    }
  }
  
  def name2bool(name:Name, truthVal: Boolean): Name => Boolean = name match{
    case _ =>name=>truthVal
  }
  
  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match { 
    case Nil => List(Nil) 
    case h :: t => for(xh <- h; xt <- cartesianProduct(t)) yield xh :: xt 
  }
  
  
  val vals = List(false,true)
  	
  def solve(goal: Proposition): Option[List[Name => Boolean]] =
  {
    val namesList:List[Name]=vars(goal).toList 
    var list:List[Name => Boolean] = List()
    if(namesList.size==0){
      if(reduce(goal)==True) Some(List(namesList=>true))
      else None
    }
    else{
      val table:List[List[Boolean]] = cartesianProduct(namesList.map(_=>vals))			//compute the full truth table
      var p:Proposition = goal
      for(j<- 0 to table.size-1){
    	  for(i<- 0 to namesList.size-1) {p = substitute(namesList(i),table(j)(i))(goal)}	//test the values from the truth table
    	  if(reduce(p)==True) {
    	    var ls:List[Name => Boolean] = List()
    	    for(i<- 0 to namesList.size-1) ls = name2bool(namesList(i),table(j)(i))::ls
    	    list = list ++ ls
    	  }
      }
      Some(list)
    }
  }
  
  import OBDD._

  def obdd(names: List[Name])(p: Proposition): OBDD =
    error("TODO")
}