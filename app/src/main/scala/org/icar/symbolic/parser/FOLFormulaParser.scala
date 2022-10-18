package org.icar.symbolic.parser

import org.icar.symbolic._
import org.icar.symbolic.builder.FOLBuilder

import scala.util.parsing.combinator.JavaTokenParsers

class FOLFormulaParser extends FOLFormulaParserTrait

trait FOLFormulaParserTrait extends JavaTokenParsers {
  var b = new FOLBuilder

  def formula : Parser[LogicFormula with FOLNature] = exist_formula | foreach_formula | and_formula

  def exist_formula : Parser[LogicFormula with FOLNature] = "exists"~variable_term~","~formula ^^ {case _~v~_~form => b.exists(v,form)}
  def foreach_formula : Parser[LogicFormula with FOLNature] = "foreach"~variable_term~","~formula ^^ {case _~v~_~form => b.foreach(v,form)}

  def and_formula : Parser[LogicFormula with FOLNature] = or_formula~"and"~not_formula ^^ {case form1~_~form2 => b.and(form1,form2)} | or_formula ^^ {x=>x}
  def or_formula : Parser[LogicFormula with FOLNature] = impl_formula~"or"~not_formula ^^ {case form1~_~form2 => b.or(form1,form2)} | impl_formula ^^ {x=>x}
  def impl_formula : Parser[LogicFormula with FOLNature] = biiml_formula~"->"~not_formula ^^ {case form1~_~form2 => b.implies(form1,form2)} | biiml_formula ^^ {x=>x}
  def biiml_formula : Parser[LogicFormula with FOLNature] = not_formula~"<->"~not_formula ^^ {case form1~_~form2 => b.biimpl(form1,form2)} | not_formula ^^ {x=>x}
  def not_formula : Parser[LogicFormula with FOLNature] = "not"~left_formula ^^ {case _~form =>b.not(form) } | left_formula

  def left_formula : Parser[LogicFormula with FOLNature] = comma_formula | "true" ^^ {_=>b.truth} | "false" ^^ {_=>b.falsity} | predicate
  def comma_formula : Parser[LogicFormula with FOLNature] = "("~formula~")" ^^ {case _~form~_ => form}

  def predicate : Parser[LogicFormula with FOLNature] =
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
  def number_term : Parser[NumberTerm] = floatingPointNumber ^^ { n => NumberTerm(n.toDouble)}
  def variable_term : Parser[VariableTerm] = "?"~ident ^^ { case _~x => VariableTerm(x)}
}


//
//object RunFOLFormulaParser extends FOLFormulaParser {
//  def main(args: Array[String]): Unit = {
//    println(parseAll(formula,"test(a,?b)").get)
//    println(parseAll(formula,"exists ?x, human(?x,y)").get)
//    println(parseAll(formula,"foreach ?color, human(x,y) and tshirt(?color)").get)
//    println(parseAll(formula,"foreach ?color, exists ?y, human(?y) and tshirt(?y,?color)").get)
//    println(parseAll(formula,"test1 and test2").get)
//    println(parseAll(formula,"test1 and (test2 or test3)").get)
//  }
//}
//

