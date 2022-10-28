package org.icar.subsymbolic_test

import org.icar.subsymbolic.builder.{SubSymbolicBuilder, UnsupportedRawConversion}
import org.icar.subsymbolic.{RawConj, RawDisj, RawNeg, RawProposition}
import org.icar.symbolic._
import org.icar.symbolic.parser.DomainOntologyParser
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SubSymBuilderSuite extends AnyFunSuite {
  val p = new DomainOntologyParser

  val onto_parser = p.parseAll(p.domain,"domain \"prova5\" {  " +
    "category users atom [ luca,john,claudia ] " +
    "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
    "category sensor_id number [ 1,2,3 ]  " +

    "define location(enum[sensor_id],enum[rooms])" +

    "}")

  val onto = onto_parser.get

  test("subsymbolic map from ontology") {
    val builder = new SubSymbolicBuilder(onto)

    assert(builder.inverse.size == 9)
  }

  test("subsymbolic predicate conversion") {
    val builder = new SubSymbolicBuilder(onto)
    val raw = builder.formula( Predicate("location",List(NumberTerm(2),StringTerm("bedroom"))), Map.empty )
    assert(raw == RawProposition(5))
  }

  test("subsymbolic conjunction conversion") {
    val builder = new SubSymbolicBuilder(onto)
    val raw = builder.formula(
      Conjunction(List(
        Predicate("location",List(NumberTerm(1),StringTerm("bedroom"))),
        Predicate("location",List(NumberTerm(2),StringTerm("bedroom"))),
        Predicate("location",List(NumberTerm(2),StringTerm("kitchen"))))
      ),Map.empty)
    assert(raw == RawConj(RawProposition(2),RawConj(RawProposition(5),RawProposition(4))))
  }

  test("subsymbolic disjunction/conjunction conversion") {
    val builder = new SubSymbolicBuilder(onto)
    val raw = builder.formula(
      Disjunction(List(
        Predicate("location",List(NumberTerm(1),StringTerm("bedroom"))),
        Conjunction(List(
          Predicate("location",List(NumberTerm(2),StringTerm("bedroom"))),
          Predicate("location",List(NumberTerm(2),StringTerm("kitchen"))))
        ))
      ),Map.empty)
    assert(raw == RawDisj(RawProposition(2),RawConj(RawProposition(5),RawProposition(4))))
  }


  test("subsymbolic negation/disjunction/conjunction conversion") {
    val builder = new SubSymbolicBuilder(onto)
    val raw = builder.formula(
      Negation(
        Disjunction(List(
          Predicate("location",List(NumberTerm(1),StringTerm("bedroom"))),
          Conjunction(List(
            Predicate("location",List(NumberTerm(2),StringTerm("bedroom"))),
            Predicate("location",List(NumberTerm(2),StringTerm("kitchen"))))
          ))
        )),Map.empty)
    assert(raw == RawNeg( RawDisj(RawProposition(2),RawConj(RawProposition(5),RawProposition(4)))) )
  }

  test("subsymbolic exists-quantifier conversion") {
    val builder = new SubSymbolicBuilder(onto)
    val sensor_id = onto.get_category_by_name("sensor_id")
    val raw = builder.formula(
      ExistQuantifier(VariableDef("x","ID"),Predicate("location",List(VariableTerm("x"),StringTerm("bedroom"))))
      ,Map("x"->sensor_id))
    assert(raw == RawDisj(RawProposition(2),RawDisj(RawProposition(5),RawProposition(8))))
  }

  test("subsymbolic univ-quantifier conversion") {
    val builder = new SubSymbolicBuilder(onto)
    val rooms = onto.get_category_by_name("rooms")
    val raw = builder.formula(
      UnivQuantifier(VariableDef("x","ID"),Predicate("location",List(NumberTerm(2),VariableTerm("x"))))
      ,Map("x"->rooms))
    assert(raw == RawConj(RawProposition(3),RawConj(RawProposition(4),RawProposition(5))))
  }

  test("subsymbolic exist-exist/conjunction conversion") {
    val builder = new SubSymbolicBuilder(onto)
    val rooms = onto.get_category_by_name("rooms")
    val sensor_id = onto.get_category_by_name("sensor_id")
    val raw = builder.formula(
      ExistQuantifier(VariableDef("x","ID"),
        ExistQuantifier(VariableDef("y","String"),
          Predicate("location",List(VariableTerm("x"),VariableTerm("y"))))
      ),Map("x"->sensor_id, "y"->rooms))
    assert(raw == RawDisj(RawDisj(RawProposition(0),RawDisj(RawProposition(1),RawProposition(2))),RawDisj(RawDisj(RawProposition(3),RawDisj(RawProposition(4),RawProposition(5))),RawDisj(RawProposition(6),RawDisj(RawProposition(7),RawProposition(8))))))
  }

  test("subsymbolic exist/foreach conversion") {
    val builder = new SubSymbolicBuilder(onto)
    val sensor_id = onto.get_category_by_name("sensor_id")
    val raw = builder.formula(
      ExistQuantifier(VariableDef("x","ID"),Conjunction(List(
        Predicate("location",List(VariableTerm("x"),StringTerm("bedroom"))),
        Predicate("location",List(VariableTerm("x"),StringTerm("kitchen")))
      ))),Map("x"->sensor_id))
    assert(raw == RawDisj(RawConj(RawProposition(2),RawProposition(1)),RawDisj(RawConj(RawProposition(5),RawProposition(4)),RawConj(RawProposition(8),RawProposition(7)))))
  }

  test("subsymbolic conversion of ltl formulas throws exceptions") {
    val builder = new SubSymbolicBuilder(onto)
    assertThrows[UnsupportedRawConversion] {
      val raw = builder.formula(Finally(Predicate("location", List(NumberTerm(2), StringTerm("bedroom")))), Map.empty)
    }
  }
}
