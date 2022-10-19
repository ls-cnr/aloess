package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.builder.DomainOntologyBuilder
import org.icar.symbolic.parser.{AbstractCapabilityParser, DomainOntologyParser}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AbstractCapabilityParserSuite extends AnyFunSuite {

  test("parsing abstract capability") {
    val p = new AbstractCapabilityParser

    val result = p.parseAll(p.capability,"capability \"cap1\" [ " +
      "params : ?x is RoomType, ?y is Location " +
      "pre: input(?x)   " +
      "post: output(b)   " +
      "evolution \"evo1\" : add output(b), rmv input(?a)   " +
      "evolution \"evo2\" : add output(b1), rmv input(?a)   " +
      "future: G output(?b)   " +
      "]")
    assert(result.successful)

    assert(result.get == AbstractCapability(
      id = "cap1",
      params = List(
        CapabilityParameter(VariableTerm("x"),"RoomType"),
        CapabilityParameter(VariableTerm("y"),"Location")),
      pre = Predicate("input",List(VariableTerm("x"))),
      post = Predicate("output",List(AtomTerm("b"))),
      effects = List(
        EvolutionGrounding("evo1",List(
          AddOperator(Predicate("output",List(AtomTerm("b")))),
          RmvOperator(Predicate("input",List(VariableTerm("a")))))),
        EvolutionGrounding("evo2",List(
          AddOperator(Predicate("output",List(AtomTerm("b1")))),
          RmvOperator(Predicate("input",List(VariableTerm("a"))))))),
      future = Globally(Predicate("output",List(VariableTerm("b"))))
    ))
  }

}


