package org.icar.symbolic_test

import org.icar.symbolic._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LTLFormulaSuite extends AnyFunSuite {
  test("building LTL formula without temporal operators") {
    val b = new LTLBuilder
    val formula = b.and(b.proposition("test",List()),b.foreach(VariableTerm("integer"), b.predicate("test",List(VariableTerm("integer")))))
    assert(formula == Conjunction(List(GroundPredicate("test",List()),UnivQuantifier(VariableTerm("integer"),Predicate("test",List(VariableTerm("integer")))))))
  }

  test("building LTL formula with Finally") {
    val b = new LTLBuilder
    val formula = b.finally_(b.and(b.proposition("test",List()),b.foreach(VariableTerm("integer"), b.predicate("test",List(VariableTerm("integer"))))))
    assert(formula == Finally(Conjunction(List(GroundPredicate("test",List()),UnivQuantifier(VariableTerm("integer"),Predicate("test",List(VariableTerm("integer"))))))))
  }

  test("building 'and' two LTL formulas") {
    val b = new LTLBuilder
    val formula = b.or(
      b.globally(b.predicate("uno",List())),
      b.until(
        b.predicate("due",List()),
        b.predicate("tre",List()))
    )
    assert(formula == Disjunction(List(Globally(Predicate("uno",List())),Until(Predicate("due",List()),Predicate("tre",List())))))
  }

}
