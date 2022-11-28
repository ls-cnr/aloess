package org.icar.domain

import org.icar.domain.RunIDS_BestFistSolver.{solver, solver_result}
import org.icar.solver.{FullSolutions, IterationTermination, PartialSolutions, SolverError}
import org.icar.solver.best_first.BestFirstSolver
import org.icar.symbolic.StateOfWorld
import org.icar.symbolic.builder.PropositionBuilder
import org.icar.symbolic.parser.{AbstractCapabilityParser, DomainOntologyParser, GoalTreeParser}

import scala.io.Source

object AAL4E {
  val domain_parser = new DomainOntologyParser
  val goal_parser = new GoalTreeParser

  val domain_string =Source.fromFile("app/src/main/resources/domain/aal4e/domain.onto").getLines().mkString

  val onto_parser_result = domain_parser.parseAll(domain_parser.domain,domain_string)
  val onto = onto_parser_result.get

  val goal_model_string =Source.fromFile("app/src/main/resources/domain/aal4e/aal4e.gm").getLines().mkString
  val goal_parser_result = goal_parser.parseAll(goal_parser.goal_tree, goal_model_string)

  val goal_model = goal_parser_result.get

  val find_user_string =Source.fromFile("app/src/main/resources/domain/aal4e/find_user.cap").getLines().mkString
  val capability_parser1 = new AbstractCapabilityParser
  val capability_parser_result = capability_parser1.parseAll(capability_parser1.capability,find_user_string)
  val find_user_cap = capability_parser_result.get

  val engage_string =Source.fromFile("app/src/main/resources/domain/aal4e/engage.cap").getLines().mkString
  val capability_parser2 = new AbstractCapabilityParser
  val capability_parser_result_2 = capability_parser2.parseAll(capability_parser2.capability,engage_string)
  val engage_cap = capability_parser_result_2.get

  val provide_social_string =Source.fromFile("app/src/main/resources/domain/aal4e/provide_social_activity.cap").getLines().mkString
  val capability_parser3 = new AbstractCapabilityParser
  val capability_parser_result_3 = capability_parser3.parseAll(capability_parser3.capability,provide_social_string)
  val provide_social_cap = capability_parser_result_3.get

  val capability_parser4 = new AbstractCapabilityParser
  val provide_cognitive_string =Source.fromFile("app/src/main/resources/domain/aal4e/provide_cognitive_exercise.cap").getLines().mkString
  val capability_parser_result_4 = capability_parser4.parseAll(capability_parser4.capability,provide_cognitive_string)
  val provide_cognitive_cap = capability_parser_result_4.get

  val capability_parser5 = new AbstractCapabilityParser
  val provide_entertainment_string =Source.fromFile("app/src/main/resources/domain/aal4e/provide_entertainment.cap").getLines().mkString
  val capability_parser_result_5 = capability_parser5.parseAll(capability_parser5.capability,provide_entertainment_string)
  val provide_entertainment_cap = capability_parser_result_5.get

  val capability_parser6 = new AbstractCapabilityParser
  val log_string =Source.fromFile("app/src/main/resources/domain/aal4e/log_activity.cap").getLines().mkString
  val capability_parser_result_6 = capability_parser6.parseAll(capability_parser6.capability,log_string)
  val log_cap = capability_parser_result_6.get

  val capability_parser7 = new AbstractCapabilityParser
  val select_content =Source.fromFile("app/src/main/resources/domain/aal4e/select_content.cap").getLines().mkString
  val capability_parser_result_7 = capability_parser7.parseAll(capability_parser7.capability,select_content)
  val select_content_cap = capability_parser_result_7.get

  val cap_repository = List( find_user_cap, engage_cap,provide_social_cap,provide_cognitive_cap,provide_entertainment_cap,select_content_cap,log_cap)

}




object RunAAL4E_BestFistSolver extends App {
  val formula_builder = new PropositionBuilder()
  val solver = new BestFirstSolver(AAL4E.onto,AAL4E.cap_repository , AAL4E.goal_model,None)

  //val user_found = formula_builder.proposition("user_location",List(AtomTerm("living_room"))).asInstanceOf[Proposition]
  val solver_result = solver.run(StateOfWorld(List()), IterationTermination(15))

  solver_result match {
    case FullSolutions(full, iterations, elapsed) =>
      println(s"*** ${full.size} FULL SOLUTIONS ($elapsed ms, $iterations its) ***")
      full.foreach( s => println(s.stringGraphviz()) )
    case PartialSolutions(partial, iterations, elapsed) =>
      println(s"*** ${solver.solution_set.size} PARTIAL SOLUTIONS ($elapsed ms, $iterations its) ***")
      println(solver.stringIterationGraphviz(6))
    case SolverError(msg, iterations, elapsed) =>
      println("*** ERROR ***")
    case _ =>
  }

}
