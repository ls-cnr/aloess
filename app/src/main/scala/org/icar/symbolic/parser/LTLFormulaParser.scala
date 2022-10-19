package org.icar.symbolic.parser

import org.icar.symbolic._
import org.icar.symbolic.builder.LTLBuilder

import scala.util.parsing.combinator.JavaTokenParsers

class LTLFormulaParser extends LTLFormulaParserTrait

trait LTLFormulaParserTrait extends JavaTokenParsers with TermParserTrait  {
  private val b = new LTLBuilder

  def ltl_formula : Parser[LogicFormula with LTLNature] = ltl_exist_formula | ltl_foreach_formula | ltl_until_formula

  def ltl_exist_formula : Parser[LogicFormula with LTLNature] = "exists"~variable_term~","~ltl_formula ^^ {case _~v~_~form => b.exists(v,form)}
  def ltl_foreach_formula : Parser[LogicFormula with LTLNature] = "foreach"~variable_term~","~ltl_formula ^^ {case _~v~_~form => b.foreach(v,form)}

  def ltl_until_formula : Parser[LogicFormula with LTLNature] = ltl_release_formula~"U"~ltl_not_formula ^^ {case form1~_~form2 => b.until(form1,form2)} | ltl_release_formula ^^ { x=>x}
  def ltl_release_formula : Parser[LogicFormula with LTLNature] = ltl_and_formula~"R"~ltl_not_formula ^^ {case form1~_~form2 => b.release(form1,form2)} | ltl_and_formula ^^ { x=>x}

  def ltl_and_formula : Parser[LogicFormula with LTLNature] = ltl_or_formula~"and"~ltl_not_formula ^^ {case form1~_~form2 => b.and(form1,form2)} | ltl_or_formula ^^ { x=>x}
  def ltl_or_formula : Parser[LogicFormula with LTLNature] = ltl_impl_formula~"or"~ltl_not_formula ^^ {case form1~_~form2 => b.or(form1,form2)} | ltl_impl_formula ^^ { x=>x}
  def ltl_impl_formula : Parser[LogicFormula with LTLNature] = ltl_biiml_formula~"->"~ltl_not_formula ^^ {case form1~_~form2 => b.implies(form1,form2)} | ltl_biiml_formula ^^ { x=>x}
  def ltl_biiml_formula : Parser[LogicFormula with LTLNature] = ltl_not_formula~"<->"~ltl_not_formula ^^ {case form1~_~form2 => b.biimpl(form1,form2)} | ltl_not_formula ^^ { x=>x}


  def ltl_not_formula : Parser[LogicFormula with LTLNature] = "not"~ltl_formula ^^ {case _~form =>b.not(form) } | ltl_left_formula

  def ltl_left_formula : Parser[LogicFormula with LTLNature] = ltl_unary_temporal_formula | ltl_comma_formula | "true" ^^ { _=>b.truth} | "false" ^^ { _=>b.falsity} | ltl_predicate

  def ltl_unary_temporal_formula : Parser[LogicFormula with LTLNature] = ltl_globally | ltl_finally | ltl_next
  def ltl_globally : Parser[LogicFormula with LTLNature] = "G"~ltl_formula ^^ {case _~form => b.globally(form)}
  def ltl_finally : Parser[LogicFormula with LTLNature] = "F"~ltl_formula ^^ {case _~form => b.finally_(form)}
  def ltl_next : Parser[LogicFormula with LTLNature] = "X"~ltl_formula ^^ {case _~form => b.next(form)}

  def ltl_comma_formula : Parser[LogicFormula with LTLNature] = "("~ltl_formula~")" ^^ {case _~form~_ => form}

  def ltl_predicate : Parser[LogicFormula with LTLNature] =
    ltl_functional~"("~ltl_args~")" ^^ {case func~_~terms~_ => b.predicate(func,terms)} |
      ltl_functional ^^ { x =>b.predicate(x,List()) }

  def ltl_functional : Parser[String] = ident
  def ltl_args : Parser[List[Term]] = repsep(ltl_term,",")

  def ltl_term : Parser[Term] = constant_term | variable_term

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



object RunLTLFormulaParser extends LTLFormulaParser {
  def main(args: Array[String]): Unit = {
    println(parseAll(ltl_formula,"F test(a,?b)").get)
    println(parseAll(ltl_formula,"exists ?x, G human(?x,y)").get)
    println(parseAll(ltl_formula,"F (foreach ?color, human(x,y) and tshirt(?color))").get)

    println(parseAll(ltl_formula,"test1 U test2").get)
    println(parseAll(ltl_formula,"(test1 and test2) and test3").get)
    println(parseAll(ltl_formula,"test1 U (test2 and test3)").get)

    println(parseAll(ltl_formula,"G (test2 and test3)").get)
    println(parseAll(ltl_formula,"F (test2 or test3)").get)


//    result match {
//      case Success(matched,_) => println(matched)
//      case Failure(msg,_) => println(s"FAILURE: $msg")
//      case Error(msg,_) => println(s"ERROR: $msg")
//    }

  }
}


