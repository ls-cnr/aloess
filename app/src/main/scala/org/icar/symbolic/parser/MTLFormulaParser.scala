package org.icar.symbolic.parser

import org.icar.symbolic._
import org.icar.symbolic.builder.MTLBuilder

import scala.util.parsing.combinator.JavaTokenParsers

class MTLFormulaParser extends MTLFormulaParserTrait

trait MTLFormulaParserTrait extends JavaTokenParsers with TermParserTrait  {
  private val b = new MTLBuilder

  def mtl_formula : Parser[LogicFormula with MTLNature] = mtl_exist_formula | mtl_foreach_formula | mtl_until_formula

  def mtl_exist_formula : Parser[LogicFormula with MTLNature] = "exists"~variable_def~","~mtl_formula ^^ {case _~v~_~form => b.exists(v.var_name,v.var_domain,form)}
  def mtl_foreach_formula : Parser[LogicFormula with MTLNature] = "foreach"~variable_def~","~mtl_formula ^^ {case _~v~_~form => b.foreach(v.var_name,v.var_domain,form)}

  def mtl_until_formula : Parser[LogicFormula with MTLNature] = mtl_release_formula~"U"~mtl_interval~mtl_not_formula ^^ {case form1~_~interv~form2 => b.until(form1,form2,interv)} | mtl_release_formula ^^ { x=>x}
  def mtl_release_formula : Parser[LogicFormula with MTLNature] = mtl_and_formula~"R"~mtl_interval~mtl_not_formula ^^ {case form1~_~interv~form2 => b.release(form1,form2,interv)} | mtl_and_formula ^^ { x=>x}

  def mtl_and_formula : Parser[LogicFormula with MTLNature] = mtl_not_formula~"and"~mtl_and_formula ^^ {case form1~_~form2 => b.and(form1,form2)} | mtl_or_formula ^^ { x=>x}
  def mtl_or_formula : Parser[LogicFormula with MTLNature] = mtl_not_formula~"or"~mtl_or_formula ^^ {case form1~_~form2 => b.or(form1,form2)} | mtl_impl_formula ^^ { x=>x}
  def mtl_impl_formula : Parser[LogicFormula with MTLNature] = mtl_not_formula~"->"~mtl_not_formula ^^ {case form1~_~form2 => b.implies(form1,form2)} | mtl_biiml_formula ^^ { x=>x}
  def mtl_biiml_formula : Parser[LogicFormula with MTLNature] = mtl_not_formula~"<->"~mtl_not_formula ^^ {case form1~_~form2 => b.biimpl(form1,form2)} | mtl_not_formula ^^ { x=>x}


  def mtl_not_formula : Parser[LogicFormula with MTLNature] = "not"~mtl_formula ^^ {case _~form =>b.not(form) } | mtl_left_formula

  def mtl_left_formula : Parser[LogicFormula with MTLNature] = mtl_unary_temporal_formula | mtl_comma_formula | "true" ^^ { _=>b.truth} | "false" ^^ { _=>b.falsity} | mtl_predicate

  def mtl_unary_temporal_formula : Parser[LogicFormula with MTLNature] = mtl_globally | mtl_finally
  def mtl_globally : Parser[LogicFormula with MTLNature] = "G"~mtl_interval~mtl_formula ^^ {case _~interv~form => b.globally(form,interv)}
  def mtl_finally : Parser[LogicFormula with MTLNature] = "F"~mtl_interval~mtl_formula ^^ {case _~interv~form => b.finally_(form,interv)}

  def mtl_comma_formula : Parser[LogicFormula with MTLNature] = "("~mtl_formula~")" ^^ {case _~form~_ => form}

  def mtl_interval : Parser[MetricInterval] = "["~wholeNumber~","~wholeNumber~"]" ^^ {case _~start~_~end~_ => MetricInterval(start.toInt,end.toInt)}

  def mtl_predicate : Parser[LogicFormula with MTLNature] =
    mtl_functional~"("~mtl_args~")" ^^ {case func~_~terms~_ => b.predicate(func,terms)} |
      mtl_functional ^^ { x =>b.predicate(x,List()) }

  def mtl_functional : Parser[String] = ident
  def mtl_args : Parser[List[Term]] = repsep(mtl_term,",")

  def mtl_term : Parser[Term] = constant_term | variable_term

//  def constant_term : Parser[ConstantTerm] =
//    "true" ^^ {_ => TrueTerm()} |
//      "false" ^^ {_ => FalseTerm()} |
//      atom_term |
//      string_term |
//      number_term
//
//  def atom_term : Parser[AtomTerm] = ident ^^ {x => AtomTerm(x)}
//  def string_term : Parser[StringTerm] = stringLiteral ^^ {x => StringTerm(x.substring(1,x.length-1))}
//  def number_term : Parser[NumberTerm] = floatingPointNumber ^^ { n => NumberTerm(n.toDouble)}
//  def variable_term : Parser[VariableTerm] = "?"~ident ^^ { case _~x => VariableTerm(x)}
}



object RunMTLFormulaParser extends MTLFormulaParser {
  def main(args: Array[String]): Unit = {
    println(parseAll(mtl_formula,"F[1,2] test(a,?b)").get)

    println(parseAll(mtl_formula,"exists ?x, G[0,15] human(?x,y)").get)
    println(parseAll(mtl_formula,"F[1,2] (foreach ?color, human(x,y) and tshirt(?color))").get)

    println(parseAll(mtl_formula,"test1 U[1,3] test2").get)
    println(parseAll(mtl_formula,"(test1 and test2) and test3").get)
    println(parseAll(mtl_formula,"test1 U[3,4] (test2 and test3)").get)

    println(parseAll(mtl_formula,"G[0,1] (test2 and test3)").get)
    println(parseAll(mtl_formula,"F[0,111] (test2 or test3)").get)

    println(parseAll(mtl_formula,"F[0,111] G[0,111] a").get)
  }
}


