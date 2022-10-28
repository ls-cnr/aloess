package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.builder.LTLBuilder
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LTLFormulaSuite extends AnyFunSuite {
  test("building LTL formula without temporal operators") {
    val b = new LTLBuilder
    val formula = b.and(b.predicate("test",List()),b.foreach("integer","Int", b.predicate("test",List(VariableTerm("integer")))))
    assert(formula == Conjunction(List(Predicate("test",List()),UnivQuantifier(VariableDef("integer","Int"),Predicate("test",List(VariableTerm("integer")))))))
  }

  test("building LTL formula with Finally") {
    val b = new LTLBuilder
    val formula = b.finally_(b.and(b.predicate("test",List()),b.foreach("integer","Int", b.predicate("test",List(VariableTerm("integer"))))))
    assert(formula == Finally(Conjunction(List(Predicate("test",List()),UnivQuantifier(VariableDef("integer","Int"),Predicate("test",List(VariableTerm("integer"))))))))
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

  test("LTL formula apply sobstitutions") {
    val b = new LTLBuilder
    val formula = b.globally(b.foreach("integer","Int", b.and(b.predicate("test1",List(VariableTerm("integer"))), b.predicate("test2",List(VariableTerm("string"))))))

    val assign_map = Map(VariableTerm("integer") -> NumberTerm(10))
    assert(formula.apply_substitution(assign_map)==Globally(
      UnivQuantifier(VariableDef("integer","Int"),
        Conjunction(List(
          Predicate("test1",List(NumberTerm(10))),
          Predicate("test2",List(VariableTerm("string"))))))
    ))
  }

}
