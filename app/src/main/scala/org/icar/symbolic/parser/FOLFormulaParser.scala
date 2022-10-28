package org.icar.symbolic.parser

import org.icar.symbolic._
import org.icar.symbolic.builder.FOLBuilder

import scala.util.parsing.combinator.JavaTokenParsers

class FOLFormulaParser extends FOLFormulaParserTrait

trait FOLFormulaParserTrait extends JavaTokenParsers with TermParserTrait  {
  private val b = new FOLBuilder

  def fol_formula : Parser[LogicFormula with FOLNature] = fol_exist_formula | fol_foreach_formula | fol_and_formula

  def fol_exist_formula : Parser[LogicFormula with FOLNature] = "exists"~variable_def~","~fol_formula ^^ {case _~v~_~form => b.exists(v.var_name,v.var_domain,form)}
  def fol_foreach_formula : Parser[LogicFormula with FOLNature] = "foreach"~variable_def~","~fol_formula ^^ {case _~v~_~form => b.foreach(v.var_name,v.var_domain,form)}

  def fol_and_formula : Parser[LogicFormula with FOLNature] = fol_or_formula~"and"~fol_not_formula ^^ {case form1~_~form2 => b.and(form1,form2)} | fol_or_formula ^^ { x=>x}
  def fol_or_formula : Parser[LogicFormula with FOLNature] = fol_impl_formula~"or"~fol_not_formula ^^ {case form1~_~form2 => b.or(form1,form2)} | fol_impl_formula ^^ { x=>x}
  def fol_impl_formula : Parser[LogicFormula with FOLNature] = fol_biiml_formula~"->"~fol_not_formula ^^ {case form1~_~form2 => b.implies(form1,form2)} | fol_biiml_formula ^^ { x=>x}
  def fol_biiml_formula : Parser[LogicFormula with FOLNature] = fol_not_formula~"<->"~fol_not_formula ^^ {case form1~_~form2 => b.biimpl(form1,form2)} | fol_not_formula ^^ { x=>x}
  def fol_not_formula : Parser[LogicFormula with FOLNature] = "not"~fol_left_formula ^^ {case _~form =>b.not(form) } | fol_left_formula

  def fol_left_formula : Parser[LogicFormula with FOLNature] = fol_comma_formula | "true" ^^ { _=>b.truth} | "false" ^^ { _=>b.falsity} | fol_predicate
  def fol_comma_formula : Parser[LogicFormula with FOLNature] = "("~fol_formula~")" ^^ {case _~form~_ => form}

  def fol_predicate : Parser[LogicFormula with FOLNature] =
    fol_functional~"("~fol_args~")" ^^ {case func~_~terms~_ => b.predicate(func,terms)} |
      fol_functional ^^ { x =>b.predicate(x,List()) }

  def fol_functional : Parser[String] = ident
  def fol_args : Parser[List[Term]] = repsep(fol_term,",")

  def fol_term : Parser[Term] = constant_term | variable_term

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

