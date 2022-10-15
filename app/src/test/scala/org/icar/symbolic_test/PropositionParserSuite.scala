package org.icar.symbolic_test

import org.icar.symbolic._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropositionParserSuite extends AnyFunSuite {
  test("parsing proposition test(a,b)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test(a,b)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Proposition("test",List(AtomTerm("a"),AtomTerm("b"))))
  }

  test("parsing all terms test(a,\"b\",10,false)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test(a,\"b\",10,false)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Proposition("test",List(AtomTerm("a"),StringTerm("b"),NumeralTerm(10),FalseTerm())))
  }

  test("parsing 1-ary proposition test") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Proposition("test",List()))
  }

    test("parsing conjunction test1(a,b) and test2(b,c)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test1(a,b) and test2(b,c)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Conjunction(List(Proposition("test1",List(AtomTerm("a"),AtomTerm("b"))),Proposition("test2",List(AtomTerm("b"),AtomTerm("c"))))))
  }

  test("parsing conjunction/1-ary/true a and (b or true)") {
    val b = new PropositionBuilder
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"a and (b or true)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==b.and(b.proposition("a"),b.or(b.proposition("b"),b.truth)))
  }

  test("parsing nested conjunction (test(a,b) and test2(a,b)) and test3(a,b)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"(test(a,b) and test2(a,b)) and test3(a,b)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Conjunction(List(
      Conjunction(List(Proposition("test",List(AtomTerm("a"),AtomTerm("b"))),Proposition("test2",List(AtomTerm("a"),AtomTerm("b"))))),
      Proposition("test3",List(AtomTerm("a"),AtomTerm("b")))
    )))
  }

  test("parsing nested conjunction test3(a,b) and (test(a,b) and test2(a,b))") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test3(a,b) and (test(a,b) and test2(a,b))")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Conjunction(List(
      Proposition("test3",List(AtomTerm("a"),AtomTerm("b"))),
      Conjunction(List(Proposition("test",List(AtomTerm("a"),AtomTerm("b"))),Proposition("test2",List(AtomTerm("a"),AtomTerm("b")))))
    )))
  }

  test("parsing negation not test(a,b)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"not test(a,b)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Negation( Proposition("test",List(AtomTerm("a"),AtomTerm("b"))) ) )
  }

  test("parsing conjunction and negation test(a,b) and not test2(a,b)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test(a,b) and not test2(a,b)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Conjunction(List(Proposition("test",List(AtomTerm("a"),AtomTerm("b"))),Negation( Proposition("test2",List(AtomTerm("a"),AtomTerm("b"))) ))) )
  }

  test("parsing conjunction and negation not test3(a,b) and not (test(a,b) and not test2(a,b))") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"not test3(a,b) and not (test(a,b) and not test2(a,b))")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==
      Conjunction(List(
        Negation( Proposition("test3",List(AtomTerm("a"),AtomTerm("b")))),
        Negation( Conjunction(List(
          Proposition("test",List(AtomTerm("a"),AtomTerm("b"))),
          Negation( Proposition("test2",List(AtomTerm("a"),AtomTerm("b"))))) )
        ))) )
  }

  test("parsing conjunction/disjunction/negation test3(a,b) or (test(a,b) and not test2(a,b))") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test3(a,b) or (test(a,b) and not test2(a,b))")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==
      Disjunction(List(
        Proposition("test3",List(AtomTerm("a"),AtomTerm("b"))),
        Conjunction(List(
          Proposition("test",List(AtomTerm("a"),AtomTerm("b"))),
          Negation( Proposition("test2",List(AtomTerm("a"),AtomTerm("b"))))) )
        )))
  }

  test("parsing conjunction/disjunction/negation2 test3(a,b) and (test(a,b) or not test2(a,b))") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test3(a,b) and (test(a,b) or not test2(a,b))")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==
      Conjunction(List(
        Proposition("test3",List(AtomTerm("a"),AtomTerm("b"))),
        Disjunction(List(
          Proposition("test",List(AtomTerm("a"),AtomTerm("b"))),
          Negation( Proposition("test2",List(AtomTerm("a"),AtomTerm("b"))))) )
      )))
  }

  test("parsing implication test1(a) -> test2(b)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test1(a) -> test2(b)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Implication(Proposition("test1",List(AtomTerm("a"))),Proposition("test2",List(AtomTerm("b")))))
  }

  test("parsing bi-implication test1(a) <-> test2(b)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"test1(a) <-> test2(b)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==BiImplication(Proposition("test1",List(AtomTerm("a"))),Proposition("test2",List(AtomTerm("b")))))
  }

  test("parsing implication and conjunction (test1(a) and test1(b)) -> test2(b)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"(test1(a) and test1(b)) -> test2(b)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Implication(
      Conjunction(List(Proposition("test1",List(AtomTerm("a"))),Proposition("test1",List(AtomTerm("b"))))),
      Proposition("test2",List(AtomTerm("b")))))
  }

  test("parsing implication,biimplication/conjunction (a and b) -> (c <-> d)") {
    val parser = new PropositionFormulaParser
    val result : parser.ParseResult[LogicFormula with PropositionNature] = parser.parseAll(parser.formula,"(a and b) -> (c <-> d)")
    assert(result.successful)
    val f : LogicFormula with PropositionNature = result.get
    assert(f==Implication(
      Conjunction(List(Proposition("a",List()),Proposition("b",List()))),
      BiImplication(Proposition("c",List()),Proposition("d",List()))
    ))
  }
}
