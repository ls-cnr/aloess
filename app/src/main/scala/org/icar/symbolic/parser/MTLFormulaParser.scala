package org.icar.symbolic.parser

import org.icar.symbolic._
import org.icar.symbolic.builder.MTLBuilder

import scala.util.parsing.combinator.JavaTokenParsers

class MTLFormulaParser extends JavaTokenParsers {
  var b = new MTLBuilder

  def formula : Parser[LogicFormula with MTLNature] = exist_formula | foreach_formula | until_formula

  def exist_formula : Parser[LogicFormula with MTLNature] = "exists"~variable_term~","~formula ^^ {case _~v~_~form => b.exists(v,form)}
  def foreach_formula : Parser[LogicFormula with MTLNature] = "foreach"~variable_term~","~formula ^^ {case _~v~_~form => b.foreach(v,form)}

  def until_formula : Parser[LogicFormula with MTLNature] = release_formula~"U"~interval~not_formula ^^ {case form1~_~interv~form2 => b.until(form1,form2,interv)} | release_formula ^^ {x=>x}
  def release_formula : Parser[LogicFormula with MTLNature] = and_formula~"R"~interval~not_formula ^^ {case form1~_~interv~form2 => b.release(form1,form2,interv)} | and_formula ^^ {x=>x}

  def and_formula : Parser[LogicFormula with MTLNature] = or_formula~"and"~not_formula ^^ {case form1~_~form2 => b.and(form1,form2)} | or_formula ^^ {x=>x}
  def or_formula : Parser[LogicFormula with MTLNature] = impl_formula~"or"~not_formula ^^ {case form1~_~form2 => b.or(form1,form2)} | impl_formula ^^ {x=>x}
  def impl_formula : Parser[LogicFormula with MTLNature] = biiml_formula~"->"~not_formula ^^ {case form1~_~form2 => b.implies(form1,form2)} | biiml_formula ^^ {x=>x}
  def biiml_formula : Parser[LogicFormula with MTLNature] = not_formula~"<->"~not_formula ^^ {case form1~_~form2 => b.biimpl(form1,form2)} | not_formula ^^ {x=>x}


  def not_formula : Parser[LogicFormula with MTLNature] = "not"~formula ^^ {case _~form =>b.not(form) } | left_formula

  def left_formula : Parser[LogicFormula with MTLNature] = unary_temporal_formula | comma_formula | "true" ^^ {_=>b.truth} | "false" ^^ {_=>b.falsity} | predicate

  def unary_temporal_formula : Parser[LogicFormula with MTLNature] = globally | finally_
  def globally : Parser[LogicFormula with MTLNature] = "G"~interval~formula ^^ {case _~interv~form => b.globally(form,interv)}
  def finally_ : Parser[LogicFormula with MTLNature] = "F"~interval~formula ^^ {case _~interv~form => b.finally_(form,interv)}

  def comma_formula : Parser[LogicFormula with MTLNature] = "("~formula~")" ^^ {case _~form~_ => form}

  def interval : Parser[MetricInterval] = "["~wholeNumber~","~wholeNumber~"]" ^^ {case _~start~_~end~_ => MetricInterval(start.toInt,end.toInt)}

  def predicate : Parser[LogicFormula with MTLNature] =
    functional~"("~args~")" ^^ {case func~_~terms~_ => b.predicate(func,terms)} |
      functional ^^ {x =>b.predicate(x,List()) }

  def functional : Parser[String] = ident
  def args : Parser[List[Term]] = repsep(term,",")

  def term : Parser[Term] = constant_term | variable_term

  def constant_term : Parser[ConstantTerm] =
    "true" ^^ {_ => TrueTerm()} |
      "false" ^^ {_ => FalseTerm()} |
      atom_term |
      string_term |
      number_term

  def atom_term : Parser[AtomTerm] = ident ^^ {x => AtomTerm(x)}
  def string_term : Parser[StringTerm] = stringLiteral ^^ {x => StringTerm(x.substring(1,x.length-1))}
  def number_term : Parser[NumeralTerm] = floatingPointNumber ^^ {n => NumeralTerm(n.toDouble)}
  def variable_term : Parser[VariableTerm] = "?"~ident ^^ { case _~x => VariableTerm(x)}
}



object RunMTLFormulaParser extends MTLFormulaParser {
  def main(args: Array[String]): Unit = {
    println(parseAll(formula,"F[1,2] test(a,?b)").get)

    println(parseAll(formula,"exists ?x, G[0,15] human(?x,y)").get)
    println(parseAll(formula,"F[1,2] (foreach ?color, human(x,y) and tshirt(?color))").get)

    println(parseAll(formula,"test1 U[1,3] test2").get)
    println(parseAll(formula,"(test1 and test2) and test3").get)
    println(parseAll(formula,"test1 U[3,4] (test2 and test3)").get)

    println(parseAll(formula,"G[0,1] (test2 and test3)").get)
    println(parseAll(formula,"F[0,111] (test2 or test3)").get)

    println(parseAll(formula,"F[0,111] G[0,111] a").get)
  }
}


