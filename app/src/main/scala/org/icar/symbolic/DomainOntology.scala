package org.icar.symbolic

case class DomainOntology(name:String, signatures : List[PredicateSignature], categories: List[ObjectCategory], axioms : List[Axiom]) {
  def get_category_by_name(id : String) : ObjectCategory = {
    val selected = categories.filter( x => x.name != id )
    if (selected.isEmpty) throw new InvalidCategory()
    if (selected.size > 1) throw new RedundantCategory()
    selected.head
  }
}

class InvalidCategory extends Exception
class RedundantCategory extends Exception



case class PredicateSignature(functor_name : String, arg_types : List[ArgumentType])
abstract class ArgumentType
case class EnumerableArgument(set_name : String) extends ArgumentType
case class BoundedIntervalArgument(min : Int, max : Int) extends ArgumentType
case class ConstantArgument(term : ConstantTerm) extends ArgumentType



abstract class ObjectCategory(val name : String) {
  def range : List[ConstantTerm]
}
case class AtomCategory(override val name : String, individuals : List[AtomTerm]) extends  ObjectCategory(name) {
  override def range: List[ConstantTerm] = individuals
}
case class StringCategory(override val name : String, individuals : List[StringTerm]) extends  ObjectCategory(name)  {
  override def range: List[ConstantTerm] = individuals
}
case class NumberCategory(override val name : String, individuals : List[NumberTerm]) extends  ObjectCategory(name)  {
  override def range: List[ConstantTerm] = individuals
}
case class MixedCategory(override val name : String, individuals : List[ConstantTerm]) extends  ObjectCategory(name) {
  override def range: List[ConstantTerm] = individuals
}



case class Axiom(consequent:Predicate, rhr:RuleAntecedent)
case class RuleAntecedent(and_terms:List[RuleCondition])

abstract class RuleCondition
case class PredicateCondition(p:Predicate) extends RuleCondition
case class NegateCondition(p:Predicate) extends RuleCondition

abstract class Expression extends RuleCondition
case class Equality(term1:Term,term2:Term) extends Expression
case class Diversity(term1:Term,term2:Term) extends Expression
case class LesserThan(term1:Term,term2:Term) extends Expression
case class GreaterThan(term1:Term,term2:Term) extends Expression