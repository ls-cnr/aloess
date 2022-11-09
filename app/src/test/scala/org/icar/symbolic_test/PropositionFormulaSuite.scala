package org.icar.symbolic_test

import org.icar.symbolic.builder.PropositionBuilder
import org.icar.symbolic.{Conjunction, Disjunction, Proposition, True}
import org.junit.Ignore
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

  test("conjunction normal form with two terms (simple)") {
    val b = new PropositionBuilder
    val p1 = b.proposition("test1")
    val p2 = b.proposition("test2")
    val p3 = b.proposition("test3")
    val formula = Conjunction(List(p1,p2,p3))
    val norm_form = formula.normal_form_two_terms
    assert(norm_form == Conjunction(List(p1,Conjunction(List(p2,p3)))))
  }

  test("conjunction normal form with two terms (complex)") {
    val b = new PropositionBuilder
    val p1 = b.proposition("test1")
    val p2 = b.proposition("test2")
    val p3 = b.proposition("test3")
    val p4 = b.proposition("test4")
    val p5 = b.proposition("test5")
    val formula = Conjunction(List(p1,p2,p3,p4,p5))
    val norm_form = formula.normal_form_two_terms
    assert(norm_form == Conjunction(List(p1,Conjunction(List(p2,Conjunction(List(p3,Conjunction(List(p4,p5)))))))))
  }

  test("disjunction normal form with two terms (simple)") {
    val b = new PropositionBuilder
    val p1 = b.proposition("test1")
    val p2 = b.proposition("test2")
    val p3 = b.proposition("test3")
    val formula = Disjunction(List(p1,p2,p3))
    val norm_form = formula.normal_form_two_terms
    assert(norm_form == Disjunction(List(p1,Disjunction(List(p2,p3)))))
  }

  test("disjunction normal form with two terms (complex)") {
    val b = new PropositionBuilder
    val p1 = b.proposition("test1")
    val p2 = b.proposition("test2")
    val p3 = b.proposition("test3")
    val p4 = b.proposition("test4")
    val p5 = b.proposition("test5")
    val formula = Disjunction(List(p1,p2,p3,p4,p5))
    val norm_form = formula.normal_form_two_terms
    assert(norm_form == Disjunction(List(p1,Disjunction(List(p2,Disjunction(List(p3,Disjunction(List(p4,p5)))))))))
  }

}
