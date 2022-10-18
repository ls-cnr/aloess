package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.builder.DomainOntologyBuilder
import org.icar.symbolic.parser.DomainOntologyParser
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DomainOntologyParserSuite extends AnyFunSuite {
  test("parsing ontology prova1") {
    val p = new DomainOntologyParser
    val b = new DomainOntologyBuilder("none")

    val result = p.parseAll(p.domain,"domain \"prova1\" {  category \"prova\" atom  [ uno,due,tre ] }")
    assert(result.successful)
    b.atom_category("prova",List("uno","due","tre"))
    assert(result.get == b.build("prova1"))
  }

  test("parsing ontology prova2") {
    val p = new DomainOntologyParser
    val b = new DomainOntologyBuilder("none")

    val result = p.parseAll(p.domain,"domain \"prova2\" {  category \"prova\" string [ \"uno\",\"due\",\"tre\" ] }")
    assert(result.successful)
    b.string_category("prova",List("uno","due","tre"))
    assert(result.get == b.build("prova2"))
  }

  test("parsing ontology prova3") {
    val p = new DomainOntologyParser
    val b = new DomainOntologyBuilder("none")

    val result = p.parseAll(p.domain,"domain \"prova3\" {  category \"prova\" number [ 1,2,3 ] }")
    assert(result.successful)
    b.number_category("prova",List(1,2,3))
    assert(result.get == b.build("prova3"))
  }

  test("parsing ontology prova4") {
    val p = new DomainOntologyParser
    val b = new DomainOntologyBuilder("none")

    val result = p.parseAll(p.domain,"domain \"prova4\" {  category \"prova\" mix [ 1,due,\"tre\" ] }")
    assert(result.successful)
    b.mixed_category("prova",List(NumberTerm(1),AtomTerm("due"),StringTerm("tre")))
    assert(result.get == b.build("prova4"))
  }

  test("parsing ontology prova5") {
    val p = new DomainOntologyParser
    val b = new DomainOntologyBuilder("none")

    val result = p.parseAll(p.domain,"domain \"prova5\" {  " +
      "category \"prova\" atom [ uno,due,tre ] " +
      "category \"prova\" string [ \"uno\",\"due\",\"tre\" ]  " +
      "category \"prova\" number [ 1,2,3 ]  " +
      "category \"prova\" mix [ 1,due,\"tre\" ]  " +
      "}")
    assert(result.successful)
    b.atom_category("prova",List("uno","due","tre"))
    b.string_category("prova",List("uno","due","tre"))
    b.number_category("prova",List(1,2,3))
    b.mixed_category("prova",List(NumberTerm(1),AtomTerm("due"),StringTerm("tre")))
    assert(result.get == b.build("prova5"))
  }

  test("parsing ontology prova6") {
    val p = new DomainOntologyParser
    val b = new DomainOntologyBuilder("none")

    val result = p.parseAll(p.domain,"domain \"prova6\" {  define \"func\"(enum[\"prova\"],interval[1,4])  }")
    assert(result.successful)
    b.signature("func").with_enum_arg("prova").with_interval_arg(1,4).create
    assert(result.get == b.build("prova6"))
  }
}
