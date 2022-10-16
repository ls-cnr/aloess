package org.icar.symbolic_test

import org.icar.symbolic.builder.PropositionBuilder
import org.icar.symbolic.{Conjunction, Proposition, True}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropositionFormulaSuite extends AnyFunSuite {
  test("building proposition formula") {
    val b = new PropositionBuilder
    val formula = b.proposition("test",List())
    assert(formula == Proposition("test",List()))
  }

  test("building always true formula") {
    val b = new PropositionBuilder
    val formula = b.truth
    assert(formula == True())
  }

  test("building 'and' formula") {
    val b = new PropositionBuilder
    val formula = b.and(b.proposition("test",List()),b.truth)
    assert(formula == Conjunction(List(Proposition("test",List()),True())))
  }

}
