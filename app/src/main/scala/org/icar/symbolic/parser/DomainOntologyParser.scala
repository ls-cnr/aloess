package org.icar.symbolic.parser

import org.icar.symbolic._
import org.icar.symbolic.builder.DomainOntologyBuilder

import scala.util.parsing.combinator.JavaTokenParsers

class DomainOntologyParser extends JavaTokenParsers with FOLFormulaParserTrait {
  val ob = new DomainOntologyBuilder("noname")

  def domain: Parser[DomainOntology] = "domain" ~stringLiteralDrimmed~ "{" ~ rep(domain_element) ~ "}" ^^ {case _~name~_~_~_ => ob.build(name) }

  def domain_element: Parser[Any] = category | signature | axiom

  def category: Parser[ObjectCategory] = atom_category | string_category | number_category | mixed_category
  def atom_category: Parser[AtomCategory] = "category" ~ ident ~ "atom" ~ "[" ~ repsep(ident, ",") ~ "]" ^^ {
    case _~cat_name~_~_~entries~_ =>ob.atom_category(cat_name,entries)
  }
  def string_category: Parser[StringCategory] = "category" ~ ident ~ "string" ~ "[" ~ repsep(stringLiteralDrimmed, ",") ~ "]" ^^ {
    case _~cat_name~_~_~entries~_ => ob.string_category(cat_name,entries)
  }
  def number_category: Parser[NumberCategory] = "category" ~ ident ~ "number" ~ "[" ~ repsep(floatingPointNumber, ",") ~ "]" ^^ {
    case _~cat_name~_~_~entries~_ =>
      val d_entries = for (s<-entries) yield s.toDouble
      ob.number_category(cat_name,d_entries)
  }
  def mixed_category: Parser[MixedCategory] = "category" ~ ident ~ "mix" ~ "[" ~ repsep(mixed, ",") ~ "]" ^^ {
    case _~cat_name~_~_~entries~_ =>
      ob.mixed_category(cat_name,entries)
  }
  def mixed: Parser[ConstantTerm] = ident ^^ {x => AtomTerm(x)} | stringLiteralDrimmed ^^ {x => StringTerm(x)} | floatingPointNumber ^^ {x => NumberTerm(x.toDouble)}

  def signature : Parser[PredicateSignature] = "define" ~ ident~"("~sign_args~")" ^^ {case _~sign~_~arg_list~_ => ob.signature(PredicateSignature(sign,arg_list))}
  def sign_args : Parser[List[ArgumentType]] = repsep(arg_definition,",")
  def arg_definition : Parser[ArgumentType] = enum_arg | interval_arg | string_arg | number_arg | atom_arg

  def enum_arg : Parser[EnumerableArgument] = "enum"~"["~ident~"]" ^^ {case _~_~lit~_ => EnumerableArgument(lit)}
  def interval_arg : Parser[BoundedIntervalArgument] = "interval"~"["~floatingPointNumber~","~floatingPointNumber~"]" ^^ { case _~_~start~_~end~_ => BoundedIntervalArgument(start.toInt,end.toInt) }
  def string_arg : Parser[ConstantArgument] = "string"~"["~stringLiteralDrimmed~"]" ^^ {case _~_~at~_ => ConstantArgument(StringTerm(at)) }
  def number_arg : Parser[ConstantArgument] = "number"~"["~floatingPointNumber~"]" ^^ {case _~_~at~_ => ConstantArgument(NumberTerm(at.toDouble)) }
  def atom_arg : Parser[ConstantArgument] = "atom"~"["~ident~"]" ^^ {case _~_~at~_ => ConstantArgument(AtomTerm(at)) }

  def axiom : Parser[Axiom] = "rule"~repsep(rule_condition,",")~"=>"~fol_predicate ^^ {
    case _~_ante~_~con => ob.axiom(con.asInstanceOf[Predicate],RuleAntecedent(_ante))
  }
//  | "rule"~repsep(rule_condition,",")~"=>"~"rmv"~fol_predicate ^^ {
//    case _~_ante~_~_~con => ob.axiom(NegateCondition(con.asInstanceOf[Predicate]),RuleAntecedent(_ante))
//  }

  def rule_condition : Parser[RuleCondition] =
    "not"~fol_predicate ^^ {
      case _~pred => NegateCondition(pred.asInstanceOf[Predicate])
    } |
    fol_predicate ^^ { x => PredicateCondition(x.asInstanceOf[Predicate]) } |
      expression

  def expression : Parser[RuleCondition] =
    fol_term~"=="~fol_term ^^ {
      case p1~_~p2 => Equality(p1,p2)
    } |
    fol_term~"!="~fol_term ^^ {
      case p1~_~p2 => Diversity(p1,p2)
  }


}




object RunOntologyOntologyParser$ extends DomainOntologyParser {
  def main(args: Array[String]): Unit = {
    println(parseAll(domain,"domain \"prova7\" {  rule p1(?a),p2(?b),?a!=?b => p12(?a,?b)  }").get)
  }
}
