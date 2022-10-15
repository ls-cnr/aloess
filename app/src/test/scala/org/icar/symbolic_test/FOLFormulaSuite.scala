package org.icar.symbolic_test

import org.icar.symbolic._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FOLFormulaSuite extends AnyFunSuite {
  test("building FOL with proposition formula") {
    val b = new FOLBuilder
    val formula : LogicFormula = b.proposition("test",List())
    assert(formula == Proposition("test",List()))
  }

  test("building FOL with predicate formula") {
    val b = new FOLBuilder
    val formula = b.predicate("test",List(VariableTerm("integer")))
    assert(formula == Predicate("test",List(VariableTerm("integer"))))
  }

  test("building 'exists' formula") {
    val b = new FOLBuilder
    val formula = b.foreach(VariableTerm("integer"), b.predicate("test",List(VariableTerm("integer"))))
    assert(formula == UnivQuantifier(VariableTerm("integer"),Predicate("test",List(VariableTerm("integer")))))
  }

  test("predicate translates to proposition") {
    val b = new FOLBuilder
    val formula = b.predicate("test",List(VariableTerm("integer")))
    val predicate = formula.asInstanceOf[Predicate]
    assert (!predicate.isGround)

    val assign_map = Map(VariableTerm("integer") -> NumeralTerm(10))
    val proposition = predicate.to_proposition(assign_map)
    assert(proposition == Proposition("test", List(NumeralTerm(10))))
  }

  test("predicate translation throws exceptions when not grounded") {
    val b = new FOLBuilder
    val formula = b.predicate("test",List(VariableTerm("integer")))
    val predicate = formula.asInstanceOf[Predicate]

    val assign_map = Map(VariableTerm("string") -> NumeralTerm(10))
    assertThrows[PredicateGroundingError] {
      val proposition = predicate.to_proposition(assign_map)
    }
  }

  test("FOL formula apply sobstitutions") {
    val b = new FOLBuilder
    val formula = b.foreach(VariableTerm("integer"), b.and(b.predicate("test1",List(VariableTerm("integer"))), b.predicate("test2",List(VariableTerm("string")))))

    val assign_map = Map(VariableTerm("integer") -> NumeralTerm(10))
    assert(formula.apply_substitution(assign_map)==
      UnivQuantifier(VariableTerm("integer"),
        Conjunction(List(
          Predicate("test1",List(NumeralTerm(10))),
          Predicate("test2",List(VariableTerm("string"))))))
    )
  }

}
