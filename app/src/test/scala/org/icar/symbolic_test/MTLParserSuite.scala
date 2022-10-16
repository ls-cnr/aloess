package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.builder.MTLBuilder
import org.icar.symbolic.parser.MTLFormulaParser
import org.junit.Ignore
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MTLParserSuite extends AnyFunSuite {
  test("parsing universal quantifier and conjunction foreach ?color, human(x,y) and tshirt(?color)") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"foreach ?color, human(x,y) and tshirt(?color)")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.foreach(VariableTerm("color"),b.and(
      b.predicate("human",List(AtomTerm("x"),AtomTerm("y"))),
      b.predicate("tshirt",List(VariableTerm("color"))))))
  }

  test("parsing metric finally F[1,2] test(a,?b)") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"F[1,2] test(a,?b)")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.finally_(b.predicate("test",List(AtomTerm("a"),VariableTerm("b"))),MetricInterval(1,2)))
  }

  test("parsing existential finally exists ?x, G[0,15] human(?x,y)") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"exists ?x, G[0,15] human(?x,y)")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.exists(VariableTerm("x"),b.globally(b.predicate("human",List(VariableTerm("x"),AtomTerm("y"))),MetricInterval(0,15))))
  }

  test("parsing finally universal conjunction F[1,2] (foreach ?color, human(x,y) and tshirt(?color))") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"F[1,2] (foreach ?color, human(x,y) and tshirt(?color))")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.finally_(b.foreach(VariableTerm("color"),b.and(
      b.predicate("human",List(AtomTerm("x"),AtomTerm("y"))),
      b.predicate("tshirt",List(VariableTerm("color"))))),MetricInterval(1,2)))
  }
  
  test("parsing until test1 U[1,3] test2") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"test1 U[1,3] test2")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.until(b.predicate("test1"),b.predicate("test2"),MetricInterval(1,3)))
  }

  test("parsing until/conjunction test1 U[3,4] (test2 and test3)") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"test1 U[3,4] (test2 and test3)")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.until(b.predicate("test1"),b.and(b.predicate("test2"),b.predicate("test3")),MetricInterval(3,4)))
  }

  test("parsing gobally/conjunction G[0,1] (test2 and test3)") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"G[0,1] (test2 and test3)")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.globally(b.and(b.predicate("test2"),b.predicate("test3")),MetricInterval(0,1)))
  }

  test("parsing finally/disjunction F[0,111] (test2 or test3)") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"F[0,111] (test2 or test3)")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.finally_(b.or(b.predicate("test2"),b.predicate("test3")),MetricInterval(0,111)))
  }

  test("parsing nested globally finally F[0,111] G[0,111] a") {
    val parser = new MTLFormulaParser
    val b = new MTLBuilder
    val result : parser.ParseResult[LogicFormula with MTLNature] = parser.parseAll(parser.formula,"F[0,111] G[0,111] a")
    assert(result.successful)
    val f : LogicFormula with MTLNature = result.get
    assert(f==b.finally_(b.globally(b.predicate("a"),MetricInterval(0,111)),MetricInterval(0,111)))
  }
}
