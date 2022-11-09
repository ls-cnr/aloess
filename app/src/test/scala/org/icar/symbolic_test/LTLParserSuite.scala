package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.builder.{FOLBuilder, LTLBuilder}
import org.icar.symbolic.parser.{FOLFormulaParser, LTLFormulaParser}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LTLParserSuite extends AnyFunSuite {
  test("parsing universal quantifier and conjunction foreach ?color, human(x,y) and tshirt(?color)") {
    val parser = new LTLFormulaParser
    val b = new LTLBuilder
    val result : parser.ParseResult[LogicFormula with LTLNature] = parser.parseAll(parser.ltl_formula,"foreach ?color in Color, human(x,y) and tshirt(?color)")
    assert(result.successful)
    val f : LogicFormula with LTLNature = result.get
    assert(f==b.foreach("color","Color",b.and(
      b.predicate("human",List(AtomTerm("x"),AtomTerm("y"))),
      b.predicate("tshirt",List(VariableTerm("color"))))))
  }

  test("parsing finally F test(a,?b)") {
    val parser = new LTLFormulaParser
    val b = new LTLBuilder
    val result : parser.ParseResult[LogicFormula with LTLNature] = parser.parseAll(parser.ltl_formula,"F test(a,?b)")
    assert(result.successful)
    val f : LogicFormula with LTLNature = result.get
    assert(f==b.finally_(b.predicate("test",List(AtomTerm("a"),VariableTerm("b")))))
  }

  test("parsing finally/existential/conjunction F (foreach ?color, human(x,y) and tshirt(?color))") {
    val parser = new LTLFormulaParser
    val b = new LTLBuilder
    val result : parser.ParseResult[LogicFormula with LTLNature] = parser.parseAll(parser.ltl_formula,"F (foreach ?color in Color, human(x,y) and tshirt(?color))")
    assert(result.successful)
    val f : LogicFormula with LTLNature = result.get
    assert(f==b.finally_(b.foreach("color","Color",b.and(
      b.predicate("human",List(AtomTerm("x"),AtomTerm("y"))),
      b.predicate("tshirt",List(VariableTerm("color")))))))
  }

  test("parsing until test1 U test2") {
    val parser = new LTLFormulaParser
    val b = new LTLBuilder
    val result : parser.ParseResult[LogicFormula with LTLNature] = parser.parseAll(parser.ltl_formula,"test1 U test2")
    assert(result.successful)
    val f : LogicFormula with LTLNature = result.get
    assert(f==b.until(b.predicate("test1"),b.predicate("test2")))
  }

  test("parsing until/conjunction test1 U (test2 and test3)") {
    val parser = new LTLFormulaParser
    val b = new LTLBuilder
    val result : parser.ParseResult[LogicFormula with LTLNature] = parser.parseAll(parser.ltl_formula,"test1 U (test2 and test3)")
    assert(result.successful)
    val f : LogicFormula with LTLNature = result.get
    assert(f==b.until(b.predicate("test1"),b.and(b.predicate("test2"),b.predicate("test3"))))
  }

  test("parsing gobally/conjunction G (test2 and test3)") {
    val parser = new LTLFormulaParser
    val b = new LTLBuilder
    val result : parser.ParseResult[LogicFormula with LTLNature] = parser.parseAll(parser.ltl_formula,"G (test2 and test3)")
    assert(result.successful)
    val f : LogicFormula with LTLNature = result.get
    assert(f==b.globally(b.and(b.predicate("test2"),b.predicate("test3"))))
  }

  test("parsing finally/disjunction F (test2 or test3)") {
    val parser = new LTLFormulaParser
    val b = new LTLBuilder
    val result : parser.ParseResult[LogicFormula with LTLNature] = parser.parseAll(parser.ltl_formula,"F (test2 or test3)")
    assert(result.successful)
    val f : LogicFormula with LTLNature = result.get
    assert(f==b.finally_(b.or(b.predicate("test2"),b.predicate("test3"))))
  }

  test("parsing finally with comma ") {
    val parser = new LTLFormulaParser
    val b = new LTLBuilder
    val result : parser.ParseResult[LogicFormula with LTLNature] = parser.parseAll(parser.ltl_formula,"F (user_engagement(not_interested) or user_engagement(social) or user_engagement(open_mind) or user_engagement(passive))")
    assert(result.successful)
  }
}
// F (user_engagement(not_interested) or user_engagement(social) or user_engagement(open_mind) or user_engagement(passive))