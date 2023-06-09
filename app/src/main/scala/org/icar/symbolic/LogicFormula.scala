package org.icar.symbolic

/* formula types */
trait PropositionNature
trait FOLNature
trait LTLNature
trait MTLNature


/* formula components */
abstract class LogicFormula {
	def isGround : Boolean
	def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]) : LogicFormula
}

case class Proposition(functional:String, terms: List[ConstantTerm] ) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString : String = functional+"("+terms.mkString(",")+")"
	def to_predicate : Predicate = Predicate(functional,for(t<-terms) yield t.asInstanceOf[Term])

	override def isGround: Boolean = true
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = this
}

case class True() extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "\u22A4"
	override def isGround: Boolean = true
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = this
}
case class False() extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "\u22A5"
	override def isGround: Boolean = true
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = this
}

case class Negation(formula : LogicFormula) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString : String = "-"+formula
	override def isGround: Boolean = formula.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = Negation(formula.apply_substitution(assignments))
}

case class Conjunction(formulas : List[LogicFormula]) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+formulas.mkString(" and ")+")"
	override def isGround: Boolean = {
		var result : Boolean = true
		for (f <- formulas) if (!f.isGround) result = false
		result
	}
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = {
		val treated_formulas = for (f <- formulas) yield f.apply_substitution(assignments)
		Conjunction(treated_formulas)
	}
	def normal_form_two_terms : Conjunction = {
		if (formulas.size<=2)
			this
		else
			Conjunction(List(formulas.head,Conjunction(formulas.tail).normal_form_two_terms))
	}
}
case class Disjunction(formulas : List[LogicFormula]) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+formulas.mkString(" or ")+")"
	override def isGround: Boolean = {
		var result : Boolean = true
		for (f <- formulas) if (!f.isGround) result = false
		result
	}
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = {
		val treated_formulas = for (f <- formulas) yield f.apply_substitution(assignments)
		Disjunction(treated_formulas)
	}
	def normal_form_two_terms : Disjunction = {
		if (formulas.size<=2)
			this
		else
			Disjunction(List(formulas.head,Disjunction(formulas.tail).normal_form_two_terms))
	}
}
case class ExclDisj(formulas : List[LogicFormula]) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+formulas.mkString(" xor ")+")"
	override def isGround: Boolean = {
		var result : Boolean = true
		for (f <- formulas) if (!f.isGround) result = false
		result
	}
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = {
		val treated_formulas = for (f <- formulas) yield f.apply_substitution(assignments)
		ExclDisj(treated_formulas)
	}
	def normal_form_two_terms : ExclDisj = {
		if (formulas.size<=2)
			this
		else
			ExclDisj(List(formulas.head,ExclDisj(formulas.tail).normal_form_two_terms))
	}
}
case class Implication(l:LogicFormula, r:LogicFormula) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+l+" -> "+r+")"
	override def isGround: Boolean = l.isGround & r.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = {
		Implication(l.apply_substitution(assignments),r.apply_substitution(assignments))
	}
}
case class BiImplication(l:LogicFormula, r:LogicFormula) extends LogicFormula with PropositionNature with FOLNature with LTLNature with MTLNature {
	override def toString: String = "("+l+" <-> "+r+")"
	override def isGround: Boolean = l.isGround & r.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = {
		BiImplication(l.apply_substitution(assignments),r.apply_substitution(assignments))
	}
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

	@throws(classOf[NotSupportedTerm])
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): Predicate = {
		var treated_terms : List[Term] = List.empty
		for (t<-terms)
			t match {
				case x : ConstantTerm => treated_terms = x :: treated_terms
				case x : VariableTerm =>
					if (assignments.contains(x))
						treated_terms = replace_var(x, assignments) :: treated_terms
					else
						treated_terms = x :: treated_terms

				case _ => throw new NotSupportedTerm("not supported term type")
			}
		Predicate(functional,treated_terms.reverse)
	}

	@throws(classOf[PredicateGroundingError])
	def to_proposition(assignments : Map[VariableTerm,ConstantTerm]) : Proposition  = {
		try {
			val ground_terms = for (t<-terms) yield replace_var(t,assignments)
			Proposition(functional,ground_terms)
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
			case NumberTerm(n) => NumberTerm(n)
			case IntegerTerm(n) => IntegerTerm(n)
			case TrueTerm() => TrueTerm()
			case FalseTerm() => FalseTerm()

			case _=> throw new NotSupportedTerm(t.getClass.getName)
		}
	}
}

case class ExistQuantifier(variable_def: VariableDef, formula : LogicFormula) extends LogicFormula with FOLNature with LTLNature with MTLNature {
	override def toString: String = "\u2203 "+variable_def.var_name+" in "+variable_def.var_domain+":"+formula
	override def isGround: Boolean = formula.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = ExistQuantifier(variable_def,formula.apply_substitution(assignments))
	def apply_category(cat : ObjectCategory) : LogicFormula = {
		val range: List[ConstantTerm] = cat.range
		val conj = for (t <- range) yield apply_substitution(Map(VariableTerm(variable_def.var_name) -> t)).asInstanceOf[ExistQuantifier].formula
		Disjunction(conj)
	}
}

case class UnivQuantifier(variable_def : VariableDef, formula : LogicFormula) extends LogicFormula with FOLNature with LTLNature with MTLNature {
	override def toString: String = "\u2200 "+variable_def.var_name+" in "+variable_def.var_domain+":"+formula
	override def isGround: Boolean = formula.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = UnivQuantifier(variable_def,formula.apply_substitution(assignments))
	def apply_category(cat : ObjectCategory) : LogicFormula = {
		val range: List[ConstantTerm] = cat.range
		val conj = for (t <- range) yield apply_substitution(Map(VariableTerm(variable_def.var_name) -> t)).asInstanceOf[UnivQuantifier].formula
		Conjunction(conj)
	}
}

case class VariableDef(var_name : String, var_domain : String)


/* LTL components */
case class Globally(formula : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+"G "+formula+")"
	override def isGround: Boolean = formula.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = Globally(formula.apply_substitution(assignments))
}
case class Finally(formula : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+"F "+formula+")"
	override def isGround: Boolean = formula.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = Finally(formula.apply_substitution(assignments))
}
case class Next(formula : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+"X "+formula+")"
	override def isGround: Boolean = formula.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = Next(formula.apply_substitution(assignments))
}
case class Until(left : LogicFormula, right : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+left+" U "+right+")"
	override def isGround: Boolean = left.isGround & right.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = Until(left.apply_substitution(assignments),right.apply_substitution(assignments))
}
case class Release(left : LogicFormula, right : LogicFormula) extends LogicFormula with LTLNature {
	override def toString: String = "("+left+" R "+right+")"
	override def isGround: Boolean = left.isGround & right.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = Release(left.apply_substitution(assignments),right.apply_substitution(assignments))
}


/* MTL components */
case class MetricGlobally(formula : LogicFormula, interval : MetricInterval) extends LogicFormula with MTLNature {
	override def toString: String = "("+"G["+interval.start+","+interval.end+"] "+formula+")"
	override def isGround: Boolean = formula.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = MetricGlobally(formula.apply_substitution(assignments),interval)
}
case class MetricFinally(formula : LogicFormula, interval : MetricInterval) extends LogicFormula with MTLNature {
	override def toString: String = "("+"F["+interval.start+","+interval.end+"] "+formula+")"
	override def isGround: Boolean = formula.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = MetricFinally(formula.apply_substitution(assignments),interval)
}
case class MetricUntil(left : LogicFormula, right : LogicFormula, interval : MetricInterval) extends LogicFormula with MTLNature {
	override def toString: String = "("+left+" U["+interval.start+","+interval.end+"] "+right+")"
	override def isGround: Boolean = left.isGround & right.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = MetricUntil(left.apply_substitution(assignments),right.apply_substitution(assignments),interval)
}
case class MetricRelease(left : LogicFormula, right : LogicFormula, interval : MetricInterval) extends LogicFormula with MTLNature {
	override def toString: String = "("+left+" R"+interval.start+","+interval.end+"] "+right+")"
	override def isGround: Boolean = left.isGround & right.isGround
	override def apply_substitution(assignments : Map[VariableTerm,ConstantTerm]): LogicFormula = MetricRelease(left.apply_substitution(assignments),right.apply_substitution(assignments),interval)
}

case class MetricInterval(start:Int,end:Int)

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
case class NumberTerm(num : Double) extends ConstantTerm {
	override def toString: String = num.toString
}
// deprecated
case class IntegerTerm(num : Int) extends ConstantTerm {
	override def toString: String = num.toString
}
case class TrueTerm() extends ConstantTerm {
	override def toString: String = "true"
}
case class FalseTerm() extends ConstantTerm {
	override def toString: String = "false"
}
case class StringTerm(str : String) extends ConstantTerm {
	override def toString: String = s"'$str'"
}


/******* EXCEPTIONS ********/
class PredicateGroundingError(s : String) extends Exception(s) {}
class NotSupportedTerm(t: String) extends Exception(t) {}
class NotSupportedLiteral(t: String) extends Exception(t) {}

