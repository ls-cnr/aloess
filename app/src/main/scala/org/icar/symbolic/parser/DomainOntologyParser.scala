package org.icar.symbolic.parser

import org.icar.symbolic.builder.DomainOntologyBuilder
import org.icar.symbolic._

import scala.util.parsing.combinator.JavaTokenParsers

class DomainOntologyParser extends JavaTokenParsers {
  val ob = new DomainOntologyBuilder("noname")

  def domain: Parser[DomainOntology] = "domain" ~stringLiteral~ "{" ~ rep(domain_element) ~ "}" ^^ {case _~name~_~_~_ => ob.build(name.substring(1,name.length-1)) }

  def domain_element: Parser[Any] = category | signature

  def category: Parser[ObjectCategory] = atom_category | string_category | number_category | mixed_category
  def atom_category: Parser[AtomCategory] = "category" ~ stringLiteral ~ "atom" ~ "[" ~ repsep(ident, ",") ~ "]" ^^ {
    case _~cat_name~_~_~entries~_ =>ob.atom_category(cat_name.substring(1,cat_name.length-1),entries)
  }
  def string_category: Parser[StringCategory] = "category" ~ stringLiteral ~ "string" ~ "[" ~ repsep(stringLiteral, ",") ~ "]" ^^ {
    case _~cat_name~_~_~entries~_ =>  val skipped_entries = for (e <- entries) yield e.substring(1,e.length-1)
      ob.string_category(cat_name.substring(1,cat_name.length-1),skipped_entries)
  }
  def number_category: Parser[NumberCategory] = "category" ~ stringLiteral ~ "number" ~ "[" ~ repsep(floatingPointNumber, ",") ~ "]" ^^ {
    case _~cat_name~_~_~entries~_ =>
      val d_entries = for (s<-entries) yield s.toDouble; ob.number_category(cat_name.substring(1,cat_name.length-1),d_entries)
  }
  def mixed_category: Parser[MixedCategory] = "category" ~ stringLiteral ~ "mix" ~ "[" ~ repsep(mixed, ",") ~ "]" ^^ {
    case _~cat_name~_~_~entries~_ =>
      ob.mixed_category(cat_name.substring(1,cat_name.length-1),entries)
  }
  def mixed: Parser[ConstantTerm] = ident ^^ {x => AtomTerm(x)} | stringLiteral ^^ {x => StringTerm(x.substring(1,x.length-1))} | floatingPointNumber ^^ {x => NumberTerm(x.toDouble)}

  def signature : Parser[PredicateSignature] = "define" ~ stringLiteral~"("~args~")" ^^ {case _~sign~_~arg_list~_ => ob.signature(PredicateSignature(sign.substring(1,sign.length-1),arg_list))}
  def args : Parser[List[ArgumentType]] = repsep(arg_definition,",")
  def arg_definition : Parser[ArgumentType] = enum_arg | interval_arg | string_arg | number_arg | atom_arg

  def enum_arg : Parser[EnumerableArgument] = "enum"~"["~stringLiteral~"]" ^^ {case _~_~lit~_ => EnumerableArgument(lit.substring(1,lit.length-1))}
  def interval_arg : Parser[BoundedIntervalArgument] = "interval"~"["~floatingPointNumber~","~floatingPointNumber~"]" ^^ { case _~_~start~_~end~_ => BoundedIntervalArgument(start.toInt,end.toInt) }
  def string_arg : Parser[ConstantArgument] = "string"~"["~stringLiteral~"]" ^^ {case _~_~at~_ => ConstantArgument(StringTerm(at.substring(1,at.length-1))) }
  def number_arg : Parser[ConstantArgument] = "number"~"["~floatingPointNumber~"]" ^^ {case _~_~at~_ => ConstantArgument(NumberTerm(at.toDouble)) }
  def atom_arg : Parser[ConstantArgument] = "atom"~"["~ident~"]" ^^ {case _~_~at~_ => ConstantArgument(AtomTerm(at)) }
}




object RunOntologyOntologyParser$ extends DomainOntologyParser {
  def main(args: Array[String]): Unit = {
    println(parseAll(domain,"domain \"prova1\" {  category \"prova\" atom  [ uno,due,tre ] }").get)
    println(parseAll(domain,"domain \"prova2\" {  category \"prova\" string [ \"uno\",\"due\",\"tre\" ] }").get)
    println(parseAll(domain,"domain \"prova3\" {  category \"prova\" number [ 1,2,3 ] }").get)
    println(parseAll(domain,"domain \"prova4\" {  category \"prova\" mix [ 1,due,\"tre\" ] }").get)
    println(parseAll(domain,"domain \"prova5\" {  " +
      "category \"prova\" atom [ uno,due,tre ] " +
      "category \"prova\" string [ \"uno\",\"due\",\"tre\" ]  " +
      "category \"prova\" number [ 1,2,3 ]  " +
      "category \"prova\" mix [ 1,due,\"tre\" ]  " +
      "}").get)

    println(parseAll(domain,"domain \"prova6\" {  define \"func\"(enum[\"prova\"],interval[1,4])  }").get)

  }
}
