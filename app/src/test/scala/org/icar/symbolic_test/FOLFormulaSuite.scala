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
    assert(formula == GroundPredicate("test",List()))
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

}
