package org.icar.symbolic.parser

import org.icar.symbolic._
import org.icar.symbolic.builder.LTLBuilder

import scala.util.parsing.combinator.JavaTokenParsers

class LTLFormulaParser extends LTLFormulaParserTrait

trait LTLFormulaParserTrait extends JavaTokenParsers {
  var b = new LTLBuilder

  def formula : Parser[LogicFormula with LTLNature] = exist_formula | foreach_formula | until_formula

  def exist_formula : Parser[LogicFormula with LTLNature] = "exists"~variable_term~","~formula ^^ {case _~v~_~form => b.exists(v,form)}
  def foreach_formula : Parser[LogicFormula with LTLNature] = "foreach"~variable_term~","~formula ^^ {case _~v~_~form => b.foreach(v,form)}

  def until_formula : Parser[LogicFormula with LTLNature] = release_formula~"U"~not_formula ^^ {case form1~_~form2 => b.until(form1,form2)} | release_formula ^^ {x=>x}
  def release_formula : Parser[LogicFormula with LTLNature] = and_formula~"R"~not_formula ^^ {case form1~_~form2 => b.release(form1,form2)} | and_formula ^^ {x=>x}

  def and_formula : Parser[LogicFormula with LTLNature] = or_formula~"and"~not_formula ^^ {case form1~_~form2 => b.and(form1,form2)} | or_formula ^^ {x=>x}
  def or_formula : Parser[LogicFormula with LTLNature] = impl_formula~"or"~not_formula ^^ {case form1~_~form2 => b.or(form1,form2)} | impl_formula ^^ {x=>x}
  def impl_formula : Parser[LogicFormula with LTLNature] = biiml_formula~"->"~not_formula ^^ {case form1~_~form2 => b.implies(form1,form2)} | biiml_formula ^^ {x=>x}
  def biiml_formula : Parser[LogicFormula with LTLNature] = not_formula~"<->"~not_formula ^^ {case form1~_~form2 => b.biimpl(form1,form2)} | not_formula ^^ {x=>x}


  def not_formula : Parser[LogicFormula with LTLNature] = "not"~formula ^^ {case _~form =>b.not(form) } | left_formula

  def left_formula : Parser[LogicFormula with LTLNature] = unary_temporal_formula | comma_formula | "true" ^^ {_=>b.truth} | "false" ^^ {_=>b.falsity} | predicate

  def unary_temporal_formula : Parser[LogicFormula with LTLNature] = globally | finally_ | next
  def globally : Parser[LogicFormula with LTLNature] = "G"~formula ^^ {case _~form => b.globally(form)}
  def finally_ : Parser[LogicFormula with LTLNature] = "F"~formula ^^ {case _~form => b.finally_(form)}
  def next : Parser[LogicFormula with LTLNature] = "X"~formula ^^ {case _~form => b.next(form)}

  def comma_formula : Parser[LogicFormula with LTLNature] = "("~formula~")" ^^ {case _~form~_ => form}

  def predicate : Parser[LogicFormula with LTLNature] =
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



object RunLTLFormulaParser extends LTLFormulaParser {
  def main(args: Array[String]): Unit = {
    println(parseAll(formula,"F test(a,?b)").get)
    println(parseAll(formula,"exists ?x, G human(?x,y)").get)
    println(parseAll(formula,"F (foreach ?color, human(x,y) and tshirt(?color))").get)

    println(parseAll(formula,"test1 U test2").get)
    println(parseAll(formula,"(test1 and test2) and test3").get)
    println(parseAll(formula,"test1 U (test2 and test3)").get)

    println(parseAll(formula,"G (test2 and test3)").get)
    println(parseAll(formula,"F (test2 or test3)").get)


//    result match {
//      case Success(matched,_) => println(matched)
//      case Failure(msg,_) => println(s"FAILURE: $msg")
//      case Error(msg,_) => println(s"ERROR: $msg")
//    }

  }
}


