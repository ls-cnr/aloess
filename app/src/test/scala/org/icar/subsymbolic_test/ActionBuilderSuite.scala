package org.icar.subsymbolic_test

import org.icar.subsymbolic.builder.{ActionBuilder, SubLogicBuilder}
import org.icar.symbolic.parser.{AbstractCapabilityParser, DomainOntologyParser}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ActionBuilderSuite extends AnyFunSuite {
  val domain_parser = new DomainOntologyParser

  val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
    "category users atom [ luca,john,claudia ] " +
    "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
    "category sensor_id number [ 1,2,3 ]  " +

    "define input(enum[users])" +
    "define output(enum[users])" +
    "define room(enum[rooms])" +

    "}")

  val onto = onto_parser.get
  val sub_logic = new SubLogicBuilder(onto)


  test("convert capability with params into actions") {
    val capability_parser = new AbstractCapabilityParser

    val result = capability_parser.parseAll(capability_parser.capability,"capability \"cap1\" [ " +
      "params : ?x is users, ?y is rooms " +
      "pre: input(?x)   " +
      "post: output(claudia)   " +
      "evolution \"evo1\" : add output(claudia), rmv room(?y)   " +
      "evolution \"evo2\" : add output(john), rmv room(?y)   " +
      "future: G output(?x)   " +
      "]")

    val cap = result.get
    val sub_actions = new ActionBuilder(sub_logic,List(cap))
    assert(sub_actions.actions.size == 9)

  }
}
