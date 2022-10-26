package org.icar.subsymbolic.builder

import org.icar.subsymbolic._
import org.icar.symbolic._
import org.icar.symbolic.parser.DomainOntologyParser

import scala.collection.mutable.ArrayBuffer

class SubSymbolicBuilder(val onto : DomainOntology) {
  var direct : Map[Proposition,Int] = Map.empty
  var inverse : ArrayBuffer[Proposition] = ArrayBuffer()
  private var var_counter : Int = 0

  init

  def init: Unit = {
    for (p <- onto.signatures) {

      var args = p.arg_types
      combine(p, args, Map.empty)

    }
  }

  private def combine(f:PredicateSignature, to_assign:List[ArgumentType], assigned:Map[ArgumentType,ConstantTerm]):Unit = {
    if (to_assign.isEmpty) {
      register(f,assigned)
    } else {
      val arg : ArgumentType = to_assign.head
      arg match {
        case BoundedIntervalArgument(min, max) => for (i <- min to max) combine(f,to_assign.tail,assigned+(arg->NumberTerm(i)))
        case ConstantArgument(term) => combine(f,to_assign.tail,assigned+(arg->term))
        case EnumerableArgument(set_name) =>
          val cats: List[ObjectCategory] = onto.categories.filter(c => c.name == set_name)
          if (cats.isEmpty)
            throw new MissingObjectCategory(set_name)
          for (value <- cats.head.range)
            combine(f,to_assign.tail,assigned+(arg->value))
        case _ => throw new UnsupportedArgumentType
      }
    }
  }

  private def register(f: PredicateSignature, assigned: Map[ArgumentType, ConstantTerm]): Unit = {

    val ground_args: List[ConstantTerm] = for (a <- f.arg_types) yield assigned(a)
    val p = Proposition(f.functor_name,ground_args)

    direct += (p->var_counter)
    inverse += p

    var_counter += 1
  }

  def formula(f : LogicFormula, sets : Map[String,ObjectCategory]) : RawLogicFormula = {
    f match {
      case p:Proposition => RawProposition(direct(p))
      case p:Predicate => formula_predicate(p)

      case True() => RawTT()
      case False() => RawFF()
      case Negation(sf) => RawNeg(formula(sf,sets))
      case Conjunction(terms) =>
        val normal = Conjunction(terms).normal_form_two_terms
        RawConj(formula(normal.formulas.head,sets),formula(normal.formulas.tail.head,sets))
      case Disjunction(terms) =>
        val normal = Disjunction(terms).normal_form_two_terms
        RawDisj(formula(normal.formulas.head,sets),formula(normal.formulas.tail.head,sets))
      case Implication(left,right) =>
        RawImpl(formula(left,sets),formula(right,sets))
      case BiImplication(left,right) =>
        RawIff(formula(left,sets),formula(right,sets))

      case ExistQuantifier(variable,form) =>
        val cat = sets(variable.name)
        val normal = ExistQuantifier(variable,form).apply_category(cat)
        formula(normal,sets)

      case UnivQuantifier(variable,form) =>
        val cat = sets(variable.name)
        val normal = UnivQuantifier(variable,form).apply_category(cat)
        formula(normal,sets)

      case _ =>
        throw new UnsupportedRawConversion(f)
    }

  }

  private def formula_predicate(p: Predicate): RawProposition = {
    if (p.isGround) {
      val prop = p.to_proposition(Map.empty)
      RawProposition(direct(prop))
    } else
      throw new PredicateMustBeGrounded(p)
  }
}


class UnsupportedArgumentType extends Exception
class MissingObjectCategory(val cat_name:String) extends Exception
class PredicateMustBeGrounded(val p : Predicate) extends Exception
class UnsupportedRawConversion(f : LogicFormula) extends Exception




object RunSubSymBuilder extends App {
  val p = new DomainOntologyParser

  val onto_parser = p.parseAll(p.domain,"domain \"prova5\" {  " +
    "category users atom [ luca,john,claudia ] " +
    "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
    "category sensor_id number [ 1,2,3 ]  " +

    "define location(enum[sensor_id],enum[rooms])" +

    "}")

  onto_parser match {
    case p.Failure(msg,_) => println(s"FAILURE: $msg")
    case p.Error(msg,_) => println(s"ERROR: $msg")

    case p.Success(matched,_) =>
      println(matched)
      val onto = onto_parser.get
      val builder = new SubSymbolicBuilder(onto)
      println(builder.inverse)

      val raw = builder.formula( Predicate("location",List(NumberTerm(2),StringTerm("bedroom"))), Map.empty )
      println(raw)

      val raw2 = builder.formula(
        Conjunction(List(
          Predicate("location",List(NumberTerm(1),StringTerm("bedroom"))),
          Predicate("location",List(NumberTerm(2),StringTerm("bedroom"))),
          Predicate("location",List(NumberTerm(2),StringTerm("kitchen"))))
        ),Map.empty)
      println(raw2)

      val raw3 = builder.formula(
        Disjunction(List(
          Predicate("location",List(NumberTerm(1),StringTerm("bedroom"))),
          Conjunction(List(
            Predicate("location",List(NumberTerm(2),StringTerm("bedroom"))),
            Predicate("location",List(NumberTerm(2),StringTerm("kitchen"))))
          ))
         ),Map.empty)
      println(raw3)

      val raw4 = builder.formula(
        Negation(
          Disjunction(List(
            Predicate("location",List(NumberTerm(1),StringTerm("bedroom"))),
            Conjunction(List(
              Predicate("location",List(NumberTerm(2),StringTerm("bedroom"))),
              Predicate("location",List(NumberTerm(2),StringTerm("kitchen"))))
            ))
        )),Map.empty)
      println(raw4)

      val sensor_id = onto.get_category_by_name("sensor_id")
      val raw5 = builder.formula(
        ExistQuantifier(VariableTerm("x"),Predicate("location",List(VariableTerm("x"),StringTerm("bedroom"))))
        ,Map("x"->sensor_id))
      println(raw5)

      val rooms = onto.get_category_by_name("rooms")
      val raw6 = builder.formula(
        UnivQuantifier(VariableTerm("x"),Predicate("location",List(NumberTerm(2),VariableTerm("x"))))
        ,Map("x"->rooms))
      println(raw6)

      val raw7 = builder.formula(
        ExistQuantifier(VariableTerm("x"),Conjunction(List(
          Predicate("location",List(VariableTerm("x"),StringTerm("bedroom"))),
          Predicate("location",List(VariableTerm("x"),StringTerm("kitchen")))
        ))),Map("x"->sensor_id))
      println(raw7)

  }

}