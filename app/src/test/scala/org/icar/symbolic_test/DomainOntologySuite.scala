package org.icar.symbolic_test

import org.icar.symbolic.builder.DomainOntologyBuilder
import org.icar.symbolic._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DomainOntologySuite extends AnyFunSuite {
  test("building ontology test") {
    val b = new DomainOntologyBuilder("test")

    val rooms = b.atom_category("rooms",List("livingroom","kitchen","bedroom"))
    b.number_category("people_number",List(1,6,8))
    b.string_category("etichette",List("stanza di luca","stanza di emilio","stanza di ale"))

    b.signature("stanza_di").with_constant_arg(StringTerm("name")).with_enum_arg(rooms).create
    b.signature("of").with_enum_arg(rooms).with_interval_arg(1,30).create

    val product = b.build()

    assert(product.categories.size==3)
    assert(product.categories.head == AtomCategory("rooms", List(AtomTerm("livingroom"), AtomTerm("kitchen"), AtomTerm("bedroom"))))
    assert(product.categories.tail.head == NumberCategory("people_number", List(NumberTerm(1.0), NumberTerm(6.0), NumberTerm(8.0))))
    assert(product.categories.tail.tail.head == StringCategory("etichette", List(StringTerm("stanza di luca"), StringTerm("stanza di emilio"), StringTerm("stanza di ale"))))

    assert(product.signatures.size==2)
    assert(product.signatures.head == PredicateSignature("stanza_di", List(ConstantArgument(StringTerm("name")), EnumerableArgument("rooms"))))

    assert(product.signatures.tail.head == PredicateSignature("of", List(EnumerableArgument("rooms"), BoundedIntervalArgument(1, 30))))
  }

}
