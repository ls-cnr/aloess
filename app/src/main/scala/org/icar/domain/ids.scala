package org.icar.domain

import org.icar.solver.best_first.BestFirstSolver
import org.icar.solver.{DomainMetric, IterationTermination}
import org.icar.subsymbolic.RawState
import org.icar.symbolic.builder.PropositionBuilder
import org.icar.symbolic.{AtomTerm, Proposition, StateOfWorld}
import org.icar.symbolic.parser.{AbstractCapabilityParser, DomainOntologyParser, GoalTreeParser}

import scala.io.Source

object IDS {
  val domain_parser = new DomainOntologyParser
  val goal_parser = new GoalTreeParser

  val domain_string =Source.fromFile("app/src/main/resources/domain/ids/domain.onto").getLines().mkString

  val onto_parser_result = domain_parser.parseAll(domain_parser.domain,domain_string)
  val onto = onto_parser_result.get

  val goal_model_string =Source.fromFile("app/src/main/resources/domain/ids/ids.gm").getLines().mkString
  val goal_parser_result = goal_parser.parseAll(goal_parser.goal_tree, goal_model_string)
  val goal_model = goal_parser_result.get

  val register_string =Source.fromFile("app/src/main/resources/domain/ids/register_doc.cap").getLines().mkString
  val capability_parser1 = new AbstractCapabilityParser
  val capability_parser_result = capability_parser1.parseAll(capability_parser1.capability,register_string)
  val register_cap = capability_parser_result.get

  val work_string =Source.fromFile("app/src/main/resources/domain/ids/work_doc.cap").getLines().mkString
  val capability_parser2 = new AbstractCapabilityParser
  val capability_parser_result_2 = capability_parser2.parseAll(capability_parser2.capability,work_string)
  val work_cap = capability_parser_result_2.get

  val revise_string =Source.fromFile("app/src/main/resources/domain/ids/revise_doc.cap").getLines().mkString
  val capability_parser3 = new AbstractCapabilityParser
  val capability_parser_result_3 = capability_parser3.parseAll(capability_parser3.capability,revise_string)
  val revise_cap = capability_parser_result_3.get

  val cap_repository = List( register_cap, work_cap, revise_cap )

  val metric = new IDSMetric
}



class IDSMetric extends DomainMetric {
  override def evaluate_state(state: RawState): Double = 0

  override def max: Double = 10
  override def min: Double = 0
}



object RunIDS_BestFistSolver extends App {
  val formula_builder = new PropositionBuilder()
  val solver = new BestFirstSolver(IDS.onto,IDS.cap_repository , IDS.goal_model, IDS.metric)

  val user_found = formula_builder.proposition("document",List(AtomTerm("issue_list"),AtomTerm("received"))).asInstanceOf[Proposition]
  val solver_result = solver.run(StateOfWorld(List(user_found)), IterationTermination(15))
  println(solver.stringIterationGraphviz(0))

}
