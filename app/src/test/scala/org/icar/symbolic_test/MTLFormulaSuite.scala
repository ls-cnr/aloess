package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.builder.MTLBuilder
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MTLFormulaSuite extends AnyFunSuite {
  test("building MTL formula without temporal operators") {
    val b = new MTLBuilder
    val formula = b.implies(b.predicate("test",List()),b.foreach("integer","Int", b.predicate("test",List(VariableTerm("integer")))))
    assert(formula == Implication( Predicate("test",List()),UnivQuantifier(VariableDef("integer","Int"),Predicate("test",List(VariableTerm("integer"))))))
  }

  test("building MTL formula with Metric Finally") {
    val b = new MTLBuilder
    val formula = b.finally_(b.and(b.predicate("test",List()),b.foreach("integer","Int", b.predicate("test",List(VariableTerm("integer"))))),MetricInterval(1,5))
    assert(formula == MetricFinally(Conjunction(List(Predicate("test",List()),UnivQuantifier(VariableDef("integer","Int"),Predicate("test",List(VariableTerm("integer")))))),MetricInterval(1,5)) )
  }

  test("building nested MTL formulas") {
    val b = new MTLBuilder
    val formula = b.globally(b.finally_(b.predicate("uno",List()),MetricInterval(1,5)), MetricInterval(1,5) )
    assert(formula == MetricGlobally(MetricFinally(Predicate("uno",List()),MetricInterval(1,5)), MetricInterval(1,5)) )
  }

  test("MTL formula apply sobstitutions") {
    val b = new MTLBuilder
    val formula = b.globally(b.foreach("integer","Int", b.and(b.predicate("test1",List(VariableTerm("integer"))), b.predicate("test2",List(VariableTerm("string"))))),MetricInterval(1,5))

    val assign_map = Map(VariableTerm("integer") -> NumberTerm(10))
    assert(formula.apply_substitution(assign_map)==MetricGlobally(
      UnivQuantifier(VariableDef("integer","Int"),
        Conjunction(List(
          Predicate("test1",List(NumberTerm(10))),
          Predicate("test2",List(VariableTerm("string")))))),MetricInterval(1,5)))
  }

}
