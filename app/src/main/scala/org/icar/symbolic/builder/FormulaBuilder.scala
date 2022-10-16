package org.icar.symbolic.builder

import org.icar.symbolic._

/* formula builder */
class PropositionBuilder {
  def proposition(functional:String, terms: List[ConstantTerm]) : LogicFormula with PropositionNature = Proposition(functional,terms)
  def proposition(functional:String) : LogicFormula with PropositionNature = Proposition(functional,List())
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
//  def proposition(functional:String, terms: List[ConstantTerm]) : LogicFormula with FOLNature = Proposition(functional,terms)
//  def proposition(functional:String) : FOLNature with PropositionNature = Proposition(functional,List())
  def truth : LogicFormula with FOLNature = True()
  def falsity : LogicFormula with FOLNature = False()
  def not(arg : LogicFormula with FOLNature) : LogicFormula with FOLNature = Negation(arg)
  def and(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = Conjunction(List(arg_left,arg_right))
  def or(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = Disjunction(List(arg_left,arg_right))
  def xor(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = ExclDisj(List(arg_left,arg_right))
  def implies(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = Implication(arg_left,arg_right)
  def biimpl(arg_left : LogicFormula with FOLNature, arg_right : LogicFormula with FOLNature) : LogicFormula with FOLNature = BiImplication(arg_left,arg_right)

  def predicate(functional:String, terms: List[Term]) : LogicFormula with FOLNature = Predicate(functional,terms)
  def predicate(functional:String) : LogicFormula with FOLNature = Predicate(functional,List())
  def exists(variable: VariableTerm, formula : LogicFormula with FOLNature) : LogicFormula with FOLNature = ExistQuantifier(variable,formula)
  def foreach(variable: VariableTerm, formula : LogicFormula with FOLNature) : LogicFormula with FOLNature = UnivQuantifier(variable,formula)

  def promote(form : LogicFormula) : LogicFormula with FOLNature = {
    form match {
      case Proposition(f,args) => predicate(f,args)
      case True() => truth
      case False() => falsity
      case Negation(arg) => not(promote(arg))
      case Conjunction(formulas) => if (formulas.length==2) and(promote(formulas.head),promote(formulas.tail.head)) else throw new NotSupportedLiteral("conjunction with > 2 args")
      case Disjunction(formulas) => if (formulas.length==2) or(promote(formulas.head),promote(formulas.tail.head)) else throw new NotSupportedLiteral("disjunction with > 2 args")
      case ExclDisj(formulas) => if (formulas.length==2) xor(promote(formulas.head),promote(formulas.tail.head)) else throw new NotSupportedLiteral("exc-disjunction with > 2 args")
      case Implication(form1,form2) =>implies(promote(form1),promote(form2))
      case BiImplication(form1,form2) =>biimpl(promote(form1),promote(form2))
      case _ => throw new NotSupportedLiteral("exc-disjunction with > 2 args")
    }
  }
}

class LTLBuilder {
//  def proposition(functional:String, terms: List[ConstantTerm]) : LogicFormula with LTLNature = Proposition(functional,terms)
//  def proposition(functional:String) : LogicFormula with LTLNature = Proposition(functional,List())
  def truth : LogicFormula with LTLNature = True()
  def falsity : LogicFormula with LTLNature = False()
  def not(arg : LogicFormula with LTLNature) : LogicFormula with LTLNature = Negation(arg)
  def and(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = Conjunction(List(arg_left,arg_right))
  def or(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = Disjunction(List(arg_left,arg_right))
  def xor(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = ExclDisj(List(arg_left,arg_right))
  def implies(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = Implication(arg_left,arg_right)
  def biimpl(arg_left : LogicFormula with LTLNature, arg_right : LogicFormula with LTLNature) : LogicFormula with LTLNature = BiImplication(arg_left,arg_right)

  def predicate(functional:String, terms: List[Term]) : LogicFormula with LTLNature = Predicate(functional,terms)
  def predicate(functional:String) : LogicFormula with LTLNature = Predicate(functional,List())
  def exists(variable: VariableTerm, formula : LogicFormula with LTLNature) : LogicFormula with LTLNature = ExistQuantifier(variable,formula)
  def foreach(variable: VariableTerm, formula : LogicFormula with LTLNature) : LogicFormula with LTLNature = UnivQuantifier(variable,formula)

  def globally(formula : LogicFormula with LTLNature) = Globally(formula)
  def finally_(formula : LogicFormula with LTLNature) = Finally(formula)
  def next(formula : LogicFormula with LTLNature) = Next(formula)
  def until(arg_left : LogicFormula with LTLNature,arg_right : LogicFormula with LTLNature) = Until(arg_left,arg_right)
  def release(arg_left : LogicFormula with LTLNature,arg_right : LogicFormula with LTLNature) = Release(arg_left,arg_right)
}


class MTLBuilder {
//  def proposition(functional:String, terms: List[ConstantTerm]) : LogicFormula with MTLNature = Proposition(functional,terms)
//  def proposition(functional:String) : LogicFormula with MTLNature = Proposition(functional,List())
  def truth : LogicFormula with MTLNature = True()
  def falsity : LogicFormula with MTLNature = False()
  def not(arg : LogicFormula with MTLNature) : LogicFormula with MTLNature = Negation(arg)
  def and(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = Conjunction(List(arg_left,arg_right))
  def or(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = Disjunction(List(arg_left,arg_right))
  def xor(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = ExclDisj(List(arg_left,arg_right))
  def implies(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = Implication(arg_left,arg_right)
  def biimpl(arg_left : LogicFormula with MTLNature, arg_right : LogicFormula with MTLNature) : LogicFormula with MTLNature = BiImplication(arg_left,arg_right)

  def predicate(functional:String, terms: List[Term]) : LogicFormula with MTLNature = Predicate(functional,terms)
  def predicate(functional:String) : LogicFormula with MTLNature = Predicate(functional,List())
  def exists(variable: VariableTerm, formula : LogicFormula with MTLNature) : LogicFormula with MTLNature = ExistQuantifier(variable,formula)
  def foreach(variable: VariableTerm, formula : LogicFormula with MTLNature) : LogicFormula with MTLNature = UnivQuantifier(variable,formula)

  def globally(formula : LogicFormula with MTLNature, interval: MetricInterval) : LogicFormula with MTLNature = MetricGlobally(formula,interval)
  def finally_(formula : LogicFormula with MTLNature, interval: MetricInterval) : LogicFormula with MTLNature = MetricFinally(formula,interval)
  def until(arg_left : LogicFormula with MTLNature,arg_right : LogicFormula with MTLNature, interval: MetricInterval): LogicFormula with MTLNature = MetricUntil(arg_left,arg_right,interval)
  def release(arg_left : LogicFormula with MTLNature,arg_right : LogicFormula with MTLNature, interval: MetricInterval) :LogicFormula with MTLNature = MetricRelease(arg_left,arg_right,interval)
}
