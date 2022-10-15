package org.icar.symbolic_test

import org.icar.symbolic._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MTLFormulaSuite extends AnyFunSuite {
  test("building MTL formula without temporal operators") {
    val b = new MTLBuilder
    val formula = b.implies(b.proposition("test",List()),b.foreach(VariableTerm("integer"), b.predicate("test",List(VariableTerm("integer")))))
    assert(formula == Implication( GroundPredicate("test",List()),UnivQuantifier(VariableTerm("integer"),Predicate("test",List(VariableTerm("integer"))))))
  }

  test("building MTL formula with Metric Finally") {
    val b = new MTLBuilder
    val formula = b.finally_(b.and(b.proposition("test",List()),b.foreach(VariableTerm("integer"), b.predicate("test",List(VariableTerm("integer"))))),MetricInterval(1,5))
    assert(formula == MetricFinally(Conjunction(List(GroundPredicate("test",List()),UnivQuantifier(VariableTerm("integer"),Predicate("test",List(VariableTerm("integer")))))),MetricInterval(1,5)) )
  }

  test("building nested MTL formulas") {
    val b = new MTLBuilder
    val formula = b.globally(b.finally_(b.predicate("uno",List()),MetricInterval(1,5)), MetricInterval(1,5) )
    assert(formula == MetricGlobally(MetricFinally(Predicate("uno",List()),MetricInterval(1,5)), MetricInterval(1,5)) )
  }

  test("MTL formula apply sobstitutions") {
    val b = new MTLBuilder
    val formula = b.globally(b.foreach(VariableTerm("integer"), b.and(b.predicate("test1",List(VariableTerm("integer"))), b.predicate("test2",List(VariableTerm("string"))))),MetricInterval(1,5))

    val assign_map = Map(VariableTerm("integer") -> NumeralTerm(10))
    assert(formula.apply_sobstitution(assign_map)==MetricGlobally(
      UnivQuantifier(VariableTerm("integer"),
        Conjunction(List(
          Predicate("test1",List(NumeralTerm(10))),
          Predicate("test2",List(VariableTerm("string")))))),MetricInterval(1,5)))
  }

}
