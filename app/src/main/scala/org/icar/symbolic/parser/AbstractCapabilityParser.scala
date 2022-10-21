package org.icar.symbolic.parser

import org.icar.symbolic._

import scala.util.parsing.combinator.JavaTokenParsers

class AbstractCapabilityParser extends JavaTokenParsers with TermParserTrait with FOLFormulaParserTrait with LTLFormulaParserTrait {
  def abstract_repository : Parser[List[AbstractCapability]] = "abstract"~"{"~rep(capability)~"}" ^^ {
    case _~_~caps~_ => caps
  }

  def capability : Parser[AbstractCapability] = "capability"~stringLiteralDrimmed~"["~opt(params)~pre~post~effects~opt(future)~"]" ^^ {
    case _~name~_~some_par~pre~post~effects~some_future~_ =>
      val params = some_par.getOrElse(List.empty)
      val fut = some_future.getOrElse(True())

      AbstractCapability(name,params,pre,post,effects,fut)
  }

  def params : Parser[List[CapabilityParameter]] = "params"~":"~repsep(capability_param,",") ^^ {case _~_~pars => pars}
  def capability_param : Parser[CapabilityParameter] = variable_term~"is"~ident ^^ {case v~_~cat => CapabilityParameter(v,cat)}

  def pre : Parser[LogicFormula with FOLNature] = "pre"~":"~fol_formula^^{case _~_~f => f}
  def post : Parser[LogicFormula with FOLNature] = "post"~":"~fol_formula ^^ {case _~_~f => f}

  def effects : Parser[List[EvolutionGrounding]] = rep(scenario)
  def scenario : Parser[EvolutionGrounding] = "evolution"~stringLiteralDrimmed~":"~repsep(evo_operator,",")~opt(probability) ^^ {
    case _~s~_~ops~None => EvolutionGrounding(s,ops)
    case _~s~_~ops~Some(pr) => EvolutionGrounding(s,ops,pr)
  }

  def probability : Parser[Double] = "with"~"prob"~floatingPointNumber ^^ {case _~_~pr => pr.toDouble}

  def evo_operator : Parser[EvoOperator] = add_predicate | remove_predicate
  def add_predicate : Parser[AddOperator] = "add"~fol_predicate ^^ {case _~op => AddOperator(op.asInstanceOf[Predicate])}
  def remove_predicate : Parser[RmvOperator] = "rmv"~fol_predicate ^^ {case _~op => RmvOperator(op.asInstanceOf[Predicate])}

  def future : Parser[LogicFormula with LTLNature] = "future"~":"~ltl_formula ^^ {case _~_~f => f}
}

object RunAbstractCapabilityParser extends App {
  val p = new AbstractCapabilityParser

  val result = p.parseAll(p.capability,"capability \"cap1\" [ " +
    "params : ?x is RoomType, ?y is Location " +
    "pre: input(?x)   " +
    "post: output(b)   " +
    "evolution \"evo1\" : add output(b), rmv input(?a)   " +
    "evolution \"evo2\" : add output(b1), rmv input(?a)   " +
    "future: G output(?b)   " +
    "]")
      result match {
        case p.Success(matched,_) => println(matched)
        case p.Failure(msg,_) => println(s"FAILURE: $msg")
        case p.Error(msg,_) => println(s"ERROR: $msg")
      }
}
