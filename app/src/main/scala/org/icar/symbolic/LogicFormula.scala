package org.icar.symbolic

/* formula types */
trait PropositionNature
trait FOLNature
trait LTLNature
trait MTLNature


/* formula components */
abstract class LogicFormula

// TODO: this class can be renamed as "Proposition"
case class GroundPredicate(functional:String, terms: List[ConstantTerm] ) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString : String = functional+"("+terms.mkString(",")+")"
	def to_predicate : Predicate = Predicate(functional,for(t<-terms) yield t.asInstanceOf[Term])
}

case class True() extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "\u22A4"
}
case class False() extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "\u22A5"
}

case class Negation(formula : LogicFormula) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString : String = "-"+formula
}
case class Conjunction(formulas : List[LogicFormula]) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+formulas.mkString(" and ")+")"
}
case class Disjunction(formulas : List[LogicFormula]) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+formulas.mkString(" or ")+")"
}
case class ExclDisj(formulas : List[LogicFormula]) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+formulas.mkString(" xor ")+")"
}
case class Implication(l:LogicFormula, r:LogicFormula) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+l+" -> "+r+")"
}
case class BiImplication(l:LogicFormula, r:LogicFormula) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+l+" <-> "+r+")"
}


/* FOL components */
case class Predicate(functional:String, terms: List[Term] ) extends LogicFormula with FOLNature with LTLNature with MTLNature {
	override def toString : String = functional+"("+terms.mkString(",")+")"

	def isGround : Boolean = {
		var ground = true
		for (t<-terms if t.isInstanceOf[VariableTerm])
			ground = false
		ground
	}

	@throws(classOf[PredicateGroundingError])
	def to_ground(assignments : Map[VariableTerm,ConstantTerm]) : GroundPredicate  = {
		try {
			val ground_terms = for (t<-terms) yield replace_var(t,assignments)
			GroundPredicate(functional,ground_terms)
		} catch {
			case e : Exception => throw new PredicateGroundingError(this.toString+" contains free variables")
		}
	}

	@throws(classOf[NotSupportedTerm])
	private def replace_var(t: Term,assignments : Map[VariableTerm,ConstantTerm]) : ConstantTerm = {
		t match {
			case VariableTerm(name) => assignments(VariableTerm(name))
			case AtomTerm(s) => AtomTerm(s)
			case StringTerm(s) => StringTerm(s)
			case NumeralTerm(n) => NumeralTerm(n)
			case IntegerTerm(n) => IntegerTerm(n)
			case TruthTerm() => TruthTerm()
			case FalsityTerm() => FalsityTerm()

			case _=> throw new NotSupportedTerm(t.getClass.getName)
		}
	}
}

case class ExistQuantifier(variable: VariableTerm, formula : LogicFormula) extends LogicFormula with FOLNature with LTLNature with MTLNature {
	override def toString: String = "\u2203 "+variable+":"+formula
}

case class UnivQuantifier(variable : VariableTerm, formula : LogicFormula) extends LogicFormula with FOLNature with LTLNature with MTLNature {
	override def toString: String = "\u2200 "+variable+":"+formula
}


/* LTL components */
case class Globally(formula : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+"G "+formula+")"
}
case class Finally(formula : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+"F "+formula+")"
}
case class Next(formula : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+"X "+formula+")"
}
case class Until(left : LogicFormula, right : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+left+" U "+right+")"
}
case class Release(left : LogicFormula, right : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+left+" R "+right+")"
}


/* MTL components */
case class MetricGlobally(formula : LogicFormula, interval : MetricInterval) extends LogicFormula with MTLNature {
	override def toString: String = "("+"G "+formula+")"
}
case class MetricFinally(formula : LogicFormula, interval : MetricInterval) extends LogicFormula with MTLNature {
	override def toString: String = "("+"F "+formula+")"
}
case class MetricUntil(left : LogicFormula, right : LogicFormula, interval : MetricInterval) extends LogicFormula with MTLNature {
	override def toString: String = "("+left+" U "+right+")"
}
case class MetricRelease(left : LogicFormula, right : LogicFormula, interval : MetricInterval) extends LogicFormula with MTLNature {
	override def toString: String = "("+left+" R "+right+")"
}




case class MetricInterval(start:Int,end:Int)
class PredicateGroundingError(s : String) extends Exception(s) {}
class NotSupportedTerm(t: String) extends Exception(t) {}

/******* PREDICATE TERMS ********/
sealed abstract class Term
case class AnonymousTerm() extends Term {
	override def toString: String = "_"
}
case class VariableTerm(name : String) extends Term {
	override def toString: String = s"var($name)"
}
abstract class ConstantTerm extends Term
case class AtomTerm(atom : String) extends ConstantTerm {
	override def toString: String = atom
}
case class NumeralTerm(num : Double) extends ConstantTerm {
	override def toString: String = num.toString
}
case class IntegerTerm(num : Int) extends ConstantTerm {
	override def toString: String = num.toString
}
case class TruthTerm() extends ConstantTerm {
	override def toString: String = "true"
}
case class FalsityTerm() extends ConstantTerm {
	override def toString: String = "false"
}
case class StringTerm(str : String) extends ConstantTerm {
	override def toString: String = s"'$str'"
}


//object HL_PredicateFormula {
//	def substitution(f:HL_PredicateFormula, assigned:Map[String,ConstantTerm]) : HL_PredicateFormula = {
//		f match {
//			case p : Predicate =>
//				val p1 = pred_substitution(p,assigned)
//				val opt_p2 = p1.get_grounded
//				if (opt_p2.isDefined)
//					opt_p2.get
//				else
//					p1
//			case p:GroundPredicate => p
//			case True() => True()
//			case False() => False()
//			case Disjunction(sf) =>
//				val subterms = for (t<-sf) yield substitution(t.asInstanceOf[HL_PredicateFormula],assigned)
//				Disjunction(subterms)
//			case Conjunction(sf) =>
//				val subterms = for (t<-sf) yield substitution(t.asInstanceOf[HL_PredicateFormula],assigned)
//				Conjunction(subterms)
//			case Negation(op) => Negation(substitution(op.asInstanceOf[HL_PredicateFormula],assigned))
//			case ExistQuantifier(vars,f) =>
//				f match {
//					case p:Predicate =>
//						var new_vars : List[VariableTerm] = List.empty
//						for (v<-vars) if (!assigned.contains(v.name)) new_vars = v :: new_vars
//						if (new_vars.nonEmpty)
//							ExistQuantifier(new_vars.reverse,substitution(p,assigned))
//						else
//							substitution(p,assigned)
//					case True() => True()
//					case False() => False()
//					case Conjunction(sf) =>
//						val sub_sf = for (s<-sf) yield substitution(ExistQuantifier(vars,s.asInstanceOf[HL_PredicateFormula]),assigned)
//						Conjunction(sub_sf)
//					case Disjunction(sf) =>
//						val sub_sf = for (s<-sf) yield substitution(ExistQuantifier(vars,s.asInstanceOf[HL_PredicateFormula]),assigned)
//						Disjunction(sub_sf)
//					case Negation(op) =>
//						val sub_op = substitution(ExistQuantifier(vars,op.asInstanceOf[HL_PredicateFormula]),assigned)
//						Negation(sub_op)
//					case _ => True()
//
//				}
//
//			case UnivQuantifier(vars,f) =>
//				f match {
//					case p:Predicate =>
//						var new_vars : List[VariableTerm] = List.empty
//						for (v<-vars) if (!assigned.contains(v.name)) new_vars = v :: new_vars
//						if (new_vars.nonEmpty)
//							UnivQuantifier(new_vars,substitution(p,assigned))
//						else
//							substitution(p,assigned)
//					case True() => True()
//					case False() => False()
//					case Conjunction(sf) =>
//						val sub_sf = for (s<-sf) yield substitution(UnivQuantifier(vars,s.asInstanceOf[HL_PredicateFormula]),assigned)
//						Conjunction(sub_sf)
//					case Disjunction(sf) =>
//						val sub_sf = for (s<-sf) yield substitution(UnivQuantifier(vars,s.asInstanceOf[HL_PredicateFormula]),assigned)
//						Disjunction(sub_sf)
//					case Negation(op) =>
//						val sub_op = substitution(UnivQuantifier(vars,op.asInstanceOf[HL_PredicateFormula]),assigned)
//						Negation(sub_op)
//					case _ => True()
//
//				}
//
//			case _ =>  True()
//		}
//	}
//
//	def pred_substitution(p:Predicate, assigned:Map[String,ConstantTerm]):Predicate = {
//		var terms_array : List[Term]=List.empty
//		for (t<-p.terms) {
//			t match {
//				case VariableTerm(name) =>
//					if (assigned.contains(name))
//						terms_array = assigned(name) :: terms_array
//					else
//						terms_array = VariableTerm(name) :: terms_array
//				case AtomTerm(n) => terms_array = AtomTerm(n) :: terms_array
//				case NumeralTerm(n) => terms_array = NumeralTerm(n) :: terms_array
//				case StringTerm(s) => terms_array = StringTerm(s) :: terms_array
//				case IntegerTerm(i)=> terms_array = IntegerTerm(i) :: terms_array
//				case _ =>
//			}
//		}
//		Predicate(p.functional,terms_array.reverse)
//	}
//}
