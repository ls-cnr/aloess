package org.icar.symbolic

/* formula builder */
class PropositionBuilder {
  def proposition(functional:String, terms: List[ConstantTerm]) : LogicFormula with PropositionNature = GroundPredicate(functional,terms)
  def truth : LogicFormula with PropositionNature = True()
  def falsity : LogicFormula with PropositionNature = False()
  def not(arg : LogicFormula with PropositionNature) : LogicFormula with PropositionNature = Negation(arg)
  def and(arg_left : LogicFormula with PropositionNature, arg_right : LogicFormula with PropositionNature) : LogicFormula with PropositionNature = Conjunction(List(arg_left,arg_right))
  def or(arg_left : LogicFormula with PropositionNature, arg_right : LogicFormula with PropositionNature) : LogicFormula with PropositionNature = Disjunction(List(arg_left,arg_right))
  def xor(arg_left : LogicFormula with PropositionNature, arg_right : LogicFormula with PropositionNature) : LogicFormula with PropositionNature = ExclDisj(List(arg_left,arg_right))
  def implies(arg_left : LogicFormula with PropositionNature, arg_right : LogicFormula with PropositionNature) : LogicFormula with PropositionNature = Implication(arg_left,arg_right)
  def biimpl(arg_left : LogicFormula with PropositionNature, arg_right : LogicFormula with PropositionNature) : LogicFormula with PropositionNature = BiImplication(arg_left,arg_right)
}

class FOLBuilder {
  def proposition(functional:String, terms: List[ConstantTerm]) : LogicFormula with FOLNature = GroundPredicate(functional,terms)
  def truth : LogicFormula with FOLNature = True()
  def falsity : LogicFormula with FOLNature = False()
  def not(arg : LogicFormula with FOLNature) : LogicFormula with FOLNature = Negation(arg)
  def and(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = Conjunction(List(arg_left,arg_right))
  def or(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = Disjunction(List(arg_left,arg_right))
  def xor(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = ExclDisj(List(arg_left,arg_right))
  def implies(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = Implication(arg_left,arg_right)
  def biimpl(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = BiImplication(arg_left,arg_right)

  def predicate(functional:String, terms: List[Term]) : LogicFormula with FOLNature = Predicate(functional,terms)
  def exists(variable: VariableTerm, formula : LogicFormula with FOLNature) : LogicFormula with FOLNature = ExistQuantifier(variable,formula)
  def foreach(variable: VariableTerm, formula : LogicFormula with FOLNature) : LogicFormula with FOLNature = UnivQuantifier(variable,formula)
}

class LTLBuilder {
  def proposition(functional:String, terms: List[ConstantTerm]) : LogicFormula with LTLNature = GroundPredicate(functional,terms)
  def truth : LogicFormula with LTLNature = True()
  def falsity : LogicFormula with LTLNature = False()
  def not(arg : LogicFormula with LTLNature) : LogicFormula with LTLNature = Negation(arg)
  def and(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = Conjunction(List(arg_left,arg_right))
  def or(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = Disjunction(List(arg_left,arg_right))
  def xor(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = ExclDisj(List(arg_left,arg_right))
  def implies(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = Implication(arg_left,arg_right)
  def biimpl(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = BiImplication(arg_left,arg_right)

  def predicate(functional:String, terms: List[Term]) : LogicFormula with LTLNature = Predicate(functional,terms)
  def exists(variable: VariableTerm, formula : LogicFormula with LTLNature) : LogicFormula with LTLNature = ExistQuantifier(variable,formula)
  def foreach(variable: VariableTerm, formula : LogicFormula with LTLNature) : LogicFormula with LTLNature = UnivQuantifier(variable,formula)

  def globally(formula : LogicFormula with LTLNature) = Globally(formula)
  def finally_(formula : LogicFormula with LTLNature) = Finally(formula)
  def next(formula : LogicFormula with LTLNature) = Next(formula)
  def until(arg_left : LogicFormula with LTLNature,arg_right : LogicFormula with LTLNature) = Until(arg_left,arg_right)
  def release(arg_left : LogicFormula with LTLNature,arg_right : LogicFormula with LTLNature) = Release(arg_left,arg_right)
}


class MTLBuilder {
  def proposition(functional:String, terms: List[ConstantTerm]) : LogicFormula with MTLNature = GroundPredicate(functional,terms)
  def truth : LogicFormula with MTLNature = True()
  def falsity : LogicFormula with MTLNature = False()
  def not(arg : LogicFormula with MTLNature) : LogicFormula with MTLNature = Negation(arg)
  def and(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = Conjunction(List(arg_left,arg_right))
  def or(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = Disjunction(List(arg_left,arg_right))
  def xor(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = ExclDisj(List(arg_left,arg_right))
  def implies(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = Implication(arg_left,arg_right)
  def biimpl(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = BiImplication(arg_left,arg_right)

  def predicate(functional:String, terms: List[Term]) : LogicFormula with MTLNature = Predicate(functional,terms)
  def exists(variable: VariableTerm, formula : LogicFormula with MTLNature) : LogicFormula with MTLNature = ExistQuantifier(variable,formula)
  def foreach(variable: VariableTerm, formula : LogicFormula with MTLNature) : LogicFormula with MTLNature = UnivQuantifier(variable,formula)

  def globally(formula : LogicFormula with MTLNature, interval: MetricInterval) : LogicFormula with MTLNature = MetricGlobally(formula,interval)
  def finally_(formula : LogicFormula with MTLNature, interval: MetricInterval) : LogicFormula with MTLNature = MetricFinally(formula,interval)
  def until(arg_left : LogicFormula with MTLNature,arg_right : LogicFormula with MTLNature, interval: MetricInterval): LogicFormula with MTLNature = MetricUntil(arg_left,arg_left,interval)
  def release(arg_left : LogicFormula with MTLNature,arg_right : LogicFormula with MTLNature, interval: MetricInterval) :LogicFormula with MTLNature = MetricRelease(arg_left,arg_left,interval)
}

//class MTLBuilder {
//  def proposition(functional:String, terms: List[ConstantTerm]) : MTLFormula = GroundPredicate(functional,terms)
//  def truth : LTLFormula = True()
//  def falsity : LTLFormula = False()
//  def not(arg : LTLFormula) : LTLFormula = Negation(arg)
//  def and(arg_left : LTLFormula,arg_right : LTLFormula) : LTLFormula = Conjunction(List(arg_left,arg_right))
//  def or(arg_left : LTLFormula,arg_right : LTLFormula) : LTLFormula = Disjunction(List(arg_left,arg_right))
//  def xor(arg_left : LTLFormula,arg_right : LTLFormula) : LTLFormula = ExclDisj(List(arg_left,arg_right))
//  def implies(arg_left : LTLFormula,arg_right : LTLFormula) : LTLFormula = Implication(List(arg_left,arg_right))
//  def biimpl(arg_left : LTLFormula,arg_right : LTLFormula) : LTLFormula = BiImplication(List(arg_left,arg_right))
//
//  def predicate(functional:String, terms: List[Term]) : LTLFormula = Predicate(functional,terms)
//  def exists(variable: VariableTerm, formula : LTLFormula) : LTLFormula = ExistQuantifier(variable,formula)
//  def foreach(variable: VariableTerm, formula : LTLFormula) : LTLFormula = UnivQuantifier(variable,formula)
//
//  def globally(formula : LTLFormula, interval: MetricInterval) = MetricGlobally(formula,interval)
//  def finally_(formula : LTLFormula, interval: MetricInterval) = MetricFinally(formula,interval)
//  def until(arg_left : LTLFormula,arg_right : LTLFormula, interval: MetricInterval) = MetricUntil(arg_left,arg_left,interval)
//  def release(arg_left : LTLFormula,arg_right : LTLFormula, interval: MetricInterval) = MetricRelease(arg_left,arg_left,interval)
//}
