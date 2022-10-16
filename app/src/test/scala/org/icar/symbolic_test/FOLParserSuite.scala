package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.builder.FOLBuilder
import org.icar.symbolic.parser.FOLFormulaParser
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FOLParserSuite extends AnyFunSuite {
  test("parsing predicate test(a,?b)") {
    val parser = new FOLFormulaParser
    val b = new FOLBuilder
    val result : parser.ParseResult[LogicFormula with FOLNature] = parser.parseAll(parser.formula,"test(a,?b)")
    assert(result.successful)
    val f : LogicFormula with FOLNature = result.get
    assert(f==b.predicate("test",List(AtomTerm("a"),VariableTerm("b"))))
  }

  test("parsing conjunction/disjunctiuon testA and (testB or testC)") {
    val parser = new FOLFormulaParser
    val b = new FOLBuilder
    val result : parser.ParseResult[LogicFormula with FOLNature] = parser.parseAll(parser.formula,"testA and (testB or testC)")
    assert(result.successful)
    val f : LogicFormula with FOLNature = result.get
    assert(f==b.and(b.predicate("testA",List()),b.or(b.predicate("testB",List()),b.predicate("testC",List()))))
  }

  test("parsing existential quantifier exists ?x, human(?x,y)") {
    val parser = new FOLFormulaParser
    val b = new FOLBuilder
    val result : parser.ParseResult[LogicFormula with FOLNature] = parser.parseAll(parser.formula,"exists ?x, human(?x,y)")
    assert(result.successful)
    val f : LogicFormula with FOLNature = result.get
    assert(f==b.exists(VariableTerm("x"),b.predicate("human",List(VariableTerm("x"),AtomTerm("y")))))
  }

  test("parsing universal quantifier and conjunction foreach ?color, human(x,y) and tshirt(?color)") {
    val parser = new FOLFormulaParser
    val b = new FOLBuilder
    val result : parser.ParseResult[LogicFormula with FOLNature] = parser.parseAll(parser.formula,"foreach ?color, human(x,y) and tshirt(?color)")
    assert(result.successful)
    val f : LogicFormula with FOLNature = result.get
    assert(f==b.foreach(VariableTerm("color"),b.and(
      b.predicate("human",List(AtomTerm("x"),AtomTerm("y"))),
      b.predicate("tshirt",List(VariableTerm("color"))))))
  }

  test("parsing universal/universal/conjunction foreach ?color, exists ?y, human(?y) and tshirt(?y,?color)") {
    val parser = new FOLFormulaParser
    val b = new FOLBuilder
    val result : parser.ParseResult[LogicFormula with FOLNature] = parser.parseAll(parser.formula,"foreach ?color, exists ?y, human(?y) and tshirt(?y,?color)")
    assert(result.successful)
    val f : LogicFormula with FOLNature = result.get
    assert(f==b.foreach(VariableTerm("color"),b.exists(VariableTerm("y"),b.and(
      b.predicate("human",List(VariableTerm("y"))),
      b.predicate("tshirt",List(VariableTerm("y"),VariableTerm("color")))))))
  }
}
