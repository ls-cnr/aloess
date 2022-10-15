package org.icar.symbolic

import scala.util.parsing.combinator.JavaTokenParsers

class PropositionFormulaParser extends JavaTokenParsers {
  val b = new PropositionBuilder

  def formula : Parser[LogicFormula with PropositionNature] = or_formula ^^ {x=>x}
  def or_formula : Parser[LogicFormula with PropositionNature] = and_formula~"or"~and_formula ^^ {case form1~_~form2 => b.or(form1,form2)} | and_formula ^^ {x=>x}
  def and_formula : Parser[LogicFormula with PropositionNature] = impl_formula~"and"~impl_formula ^^ {case form1~_~form2 => b.and(form1,form2)} | impl_formula ^^ {x=>x}
  def impl_formula : Parser[LogicFormula with PropositionNature] = biiml_formula~"->"~biiml_formula ^^ {case form1~_~form2 => b.implies(form1,form2)} | biiml_formula ^^ {x=>x}
  def biiml_formula : Parser[LogicFormula with PropositionNature] = not_formula~"<->"~not_formula ^^ {case form1~_~form2 => b.biimpl(form1,form2)} | not_formula ^^ {x=>x}
  def not_formula : Parser[LogicFormula with PropositionNature] = "not"~left_formula ^^ {case _~form =>b.not(form) } | left_formula
  def left_formula : Parser[LogicFormula with PropositionNature] = comma_formula | "true" ^^ {_=>b.truth} | "false" ^^ {_=>b.falsity} | proposition
  def comma_formula : Parser[LogicFormula with PropositionNature] = "("~formula~")" ^^ {case _~form~_ => form}

  def proposition : Parser[LogicFormula with PropositionNature] =
    functional~"("~args~")" ^^ {case func~_~terms~_ => Proposition(func,terms)} |
      functional ^^ {x =>b.proposition(x,List()) }

  def functional : Parser[String] = ident

  def args : Parser[List[ConstantTerm]] = repsep(constant_term,",")

  def constant_term : Parser[ConstantTerm] =
    "true" ^^ {_ => TrueTerm()} |
      "false" ^^ {_ => FalseTerm()} |
      atom_term |
      string_term |
      number_term

  def atom_term : Parser[AtomTerm] = ident ^^ {x => AtomTerm(x)}

  def string_term : Parser[StringTerm] = stringLiteral ^^ {x => StringTerm(x.substring(1,x.length-1))}

  def number_term : Parser[NumeralTerm] = floatingPointNumber ^^ {n => NumeralTerm(n.toDouble)}
}

/*
object TestPropositionFormulaParser extends PropositionFormulaParser {
  def main(args: Array[String]): Unit = {
    println(parseAll(formula,"test(a,b)").get)
    println(parseAll(formula,"test").get)
    println(parseAll(formula,"testA and testB").get)
    println(parseAll(formula,"test(a,b) and test2(a,b)").get)
    println(parseAll(formula,"(test(a,b) and test2(a,b)) and test3(a,b)").get)
    println(parseAll(formula,"test3(a,b) and (test(a,b) and test2(a,b))").get)
    println(parseAll(formula,"not test(a,b)").get)
    println(parseAll(formula,"test(a,b) and not test2(a,b)").get)
    println(parseAll(formula,"not test3(a,b) and not (test(a,b) and not test2(a,b))").get)
    println(parseAll(formula,"test3(a,b) or (test(a,b) and not test2(a,b))").get)
    println(parseAll(formula,"test3(a,b) and (test(a,b) or not test2(a,b))").get)
    println(parseAll(formula,"test1(a) -> test2(b)").get)
    println(parseAll(formula,"(test1(a) and test1(b)) -> test2(b)").get)
    println(parseAll(formula,"(a and b) -> (c <-> d)").get)
    println(parseAll(formula,"a and (b or true)").get)
  }
}
*/
