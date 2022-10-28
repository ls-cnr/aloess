package org.icar.symbolic.parser

import org.icar.symbolic._
import org.icar.symbolic.builder.PropositionBuilder

import scala.util.parsing.combinator.JavaTokenParsers

trait TermParserTrait extends JavaTokenParsers {
  def stringLiteralDrimmed : Parser[String] = stringLiteral ^^ {x => x.substring(1,x.length-1)}

  def constant_term : Parser[ConstantTerm] =
    "true" ^^ {_ => TrueTerm()} |
      "false" ^^ {_ => FalseTerm()} |
      atom_term |
      string_term |
      number_term

  def atom_term : Parser[AtomTerm] = ident ^^ {x => AtomTerm(x)}
  def string_term : Parser[StringTerm] = stringLiteralDrimmed ^^ {x => StringTerm(x)}
  def number_term : Parser[NumberTerm] = floatingPointNumber ^^ { n => NumberTerm(n.toDouble)}
  def variable_term : Parser[VariableTerm] = "?"~ident ^^ { case _~x => VariableTerm(x)}

  def variable_def : Parser[VariableDef] = variable_term~"in"~ident ^^ {case v~_~domain => VariableDef(v.name,domain)}
}
