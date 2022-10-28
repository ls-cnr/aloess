package org.icar.subsymbolic_test

import org.icar.subsymbolic.builder.SubLogicBuilder
import org.icar.subsymbolic._
import org.icar.symbolic.parser.DomainOntologyParser
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SubSymLTLSuite extends AnyFunSuite {
  val p = new DomainOntologyParser

  val onto_parser = p.parseAll(p.domain,"domain \"prova5\" {  " +
    "category users atom [ luca,john,claudia ] " +
    "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
    "category sensor_id number [ 1,2,3 ]  " +

    "define location(enum[sensor_id],enum[rooms])" +

    "}")
  val builder = new SubLogicBuilder(onto_parser.get)

  test("subsymbolic ltl finally formula supervisor") {
    val form = RawFinally(RawProposition(0))

    val current = RawState(Array(false,false,false,false,false,false,false,false,false))
    val next = form.next(current)
    assert(next.success_until_now == true)
    assert(next.future_formula == RawUntil(RawTT(),RawProposition(0)))

    val current2 = RawState(Array(true,false,false,false,false,false,false,false,false))
    val next2 = next.future_formula.next(current2)
    assert(next2.success_until_now == true)
    assert(next2.future_formula == RawTT())

  }

  test("subsymbolic ltl finally conjunction formula supervisor") {
    val current = RawState(Array(false,false,false,false,false,false,false,false,false))
    val form = RawFinally(RawConj(RawProposition(0),RawProposition(1)))
    val next = form.next(current)
    assert(next.success_until_now == true)
    assert(next.future_formula == RawUntil(RawTT(),RawConj(RawProposition(0),RawProposition(1))))

    val current2 = RawState(Array(true,false,false,false,false,false,false,false,false))
    val next2 = next.future_formula.next(current2)
    assert(next2.success_until_now == true)
    assert(next2.future_formula == RawUntil(RawTT(),RawConj(RawProposition(0),RawProposition(1))))

    val current3 = RawState(Array(true,true,false,false,false,false,false,false,false))
    val next3 = next2.future_formula.next(current3)
    assert(next3.success_until_now == true)
    assert(next3.future_formula == RawTT())
  }


  test("subsymbolic ltl globally conjunction formula supervisor") {
    val current = RawState(Array(true,true,false,false,false,false,false,false,false))
    val form = RawGlobally(RawConj(RawProposition(0),RawProposition(1)))
    val next = form.next(current)
    assert(next.success_until_now == true)
    assert(next.future_formula == RawRelease(RawNeg(RawTT()), RawNeg(RawNeg(RawConj(RawProposition(0),RawProposition(1))))))

    val current2 = RawState(Array(true,false,false,false,false,false,false,false,false))
    val next2 = next.future_formula.next(current2)
    assert(next2.success_until_now == false)
  }

  test("subsymbolic ltl globally disjunction formula supervisor") {
    val current = RawState(Array(true,true,false,false,false,false,false,false,false))
    val form = RawGlobally(RawDisj(RawProposition(0),RawProposition(1)))
    val next = form.next(current)
    assert(next.success_until_now == true)

    val current2 = RawState(Array(true,false,false,false,false,false,false,false,false))
    val next2 = next.future_formula.next(current2)
    assert(next2.success_until_now == true)

    val current3 = RawState(Array(false,false,false,false,false,false,false,false,false))
    val next3 = next2.future_formula.next(current3)
    assert(next3.success_until_now == false)
  }

}
