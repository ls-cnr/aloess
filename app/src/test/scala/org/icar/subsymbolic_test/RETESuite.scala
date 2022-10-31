package org.icar.subsymbolic_test

import org.icar.subsymbolic.builder.{RETEBuilder, SubLogicBuilder}
import org.icar.subsymbolic._
import org.icar.symbolic.parser.DomainOntologyParser
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RETESuite extends AnyFunSuite {


  test("convert axioms into rete") {
    val domain_parser = new DomainOntologyParser

    val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
      "category users atom [ luca,john,claudia ] " +
      "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
      "category sensor_id number [ 1,2,3 ]  " +

      "define input(enum[users])" +
      "define output(enum[users])" +
      "define room(enum[rooms])" +

      "rule input(?a), output(?b) => room(\"kitchen\")" +

      "}")

    val onto = onto_parser.get
    val logic_builder = new SubLogicBuilder(onto)
    val rete_builder = new RETEBuilder(logic_builder,onto.axioms)

    val myrete = rete_builder.rete
    assert(myrete.alphas.size==6)
    assert(myrete.betas.size==9)
    assert(myrete.prods.size==1)

  }

  test("add/rmv facts to rete") {
    val domain_parser = new DomainOntologyParser

    val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
      "category users atom [ luca,john,claudia ] " +
      "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
      "category sensor_id number [ 1,2,3 ]  " +

      "define input(enum[users])" +
      "define output(enum[users])" +
      "define room(enum[rooms])" +

      "rule input(?a), output(?b) => room(\"kitchen\")" +

      "}")

    val onto = onto_parser.get
    val logic_builder = new SubLogicBuilder(onto)
    val rete_builder = new RETEBuilder(logic_builder,onto.axioms)

    val myrete = rete_builder.rete
    val wi = RawState(Array(false,false,false,false,false,false,false,false,false))
    val start_memory = myrete.reset_memory(wi)
    val updated_memory1 = myrete.add_fact(start_memory,RawProposition(0))

    assert(updated_memory1.stable_state.satisfies(0)==true)

    val updated_memory2 = myrete.add_fact(updated_memory1,RawProposition(4))

    assert(updated_memory2.stable_state.satisfies(4))
    assert(updated_memory2.stable_state.satisfies(7))

    val updated_memory3 = myrete.rmv_fact(updated_memory2,RawProposition(4))

    assert(!updated_memory3.stable_state.satisfies(4))
    assert(!updated_memory3.stable_state.satisfies(7))
  }

  test("add/rmv facts to rete with negative conditions") {
    val domain_parser = new DomainOntologyParser

    val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
      "category users atom [ luca,john,claudia ] " +
      "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
      "category sensor_id number [ 1,2,3 ]  " +

      "define input(enum[users])" +
      "define output(enum[users])" +
      "define room(enum[rooms])" +

      "rule input(?a), not output(?b) => room(\"kitchen\")" +

      "}")

    val onto = onto_parser.get
    val logic_builder = new SubLogicBuilder(onto)
    val rete_builder = new RETEBuilder(logic_builder,onto.axioms)

    val myrete = rete_builder.rete
    val wi = RawState(Array(false,false,false,false,false,false,false,false,false))
    val start_memory = myrete.reset_memory(wi)
    val updated_memory1 = myrete.add_fact(start_memory,RawProposition(0))

    assert(updated_memory1.stable_state.satisfies(0)==true)
    assert(updated_memory1.stable_state.satisfies(7)==true)

    val updated_memory2 = myrete.rmv_fact(updated_memory1,RawProposition(0))

    assert(!updated_memory2.stable_state.satisfies(0))
    assert(!updated_memory2.stable_state.satisfies(7))
  }

  test("apply action evolution to rete") {
    val domain_parser = new DomainOntologyParser

    val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
      "category users atom [ luca,john,claudia ] " +
      "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
      "category sensor_id number [ 1,2,3 ]  " +

      "define input(enum[users])" +
      "define output(enum[users])" +
      "define room(enum[rooms])" +

      "rule input(?a), output(?b) => room(\"kitchen\")" +

      "}")

    val onto = onto_parser.get
    val logic_builder = new SubLogicBuilder(onto)
    val rete_builder = new RETEBuilder(logic_builder,onto.axioms)

    val myrete = rete_builder.rete
    val wi = RawState(Array(false,false,false,false,false,true,false,false,false))
    val start_memory = myrete.reset_memory(wi)

    println(start_memory)

    val evo = RawEvolution("act",List(RawAdd(RawProposition(0)),RawAdd(RawProposition(4)),RawRem(RawProposition(5))),0)
    val updated_memory = myrete.evolution(start_memory,evo)
    println(updated_memory)
    assert(updated_memory.stable_state.satisfies(0))
    assert(updated_memory.stable_state.satisfies(4))
    assert(!updated_memory.stable_state.satisfies(5))
    assert(updated_memory.stable_state.satisfies(7))
  }


}
