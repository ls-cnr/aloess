package org.icar.subsymbolic.builder

import org.icar.subsymbolic.rete.{RETE, ReteNode}
import org.icar.symbolic.parser.DomainOntologyParser
import org.icar.symbolic._

class RETEBuilder(val builder : SubLogicBuilder, axioms : List[Axiom]) {
  var rete = new RETE

  init

  def init : Unit = {
    for(rule <- axioms) {
      //var func_list:List[Predicate] = List.empty
      var func_list:List[RuleCondition] = List.empty
      var var_types:Map[VariableTerm,ArgumentType] = Map.empty

      for (cond <- rule.rhr.and_terms) {
        func_list = cond :: func_list

        cond match {
          case expression: Expression => throw new UnsupportedConditionType
          case NegateCondition(p) =>
            val signature = builder.onto.get_signature_by_functor(p.functional)
            for (ind <- 0 to signature.arg_types.size-1) {
              val term = p.terms(ind)
              val arg_type = signature.arg_types(ind)
              if (term.isInstanceOf[VariableTerm]) {
                var_types = var_types + (term.asInstanceOf[VariableTerm] -> arg_type)
              }
            }

          case PredicateCondition(p) =>
            val signature = builder.onto.get_signature_by_functor(p.functional)
            for (ind <- 0 to signature.arg_types.size-1) {
              val term = p.terms(ind)
              val arg_type = signature.arg_types(ind)
              if (term.isInstanceOf[VariableTerm]) {
                var_types = var_types + (term.asInstanceOf[VariableTerm] -> arg_type)
              }
            }

          case _ =>
        }
      }
      combine_rule_variables(func_list,var_types,Map.empty,rule.consequent)

    }
  }


  /**
   * questo metodo ricorsivo trova tutte le combinazioni di assegnazione per le variabili che compongono una regola
   * per ogni combinazione trovata registra un nodo predicato
   **/
  private def combine_rule_variables(antecedents:List[RuleCondition], to_assign:Map[VariableTerm,ArgumentType], assigned:Map[VariableTerm,ConstantTerm], cons : Predicate):Unit = {
    if (to_assign.isEmpty) {

      register_pred_nodes(antecedents,assigned,cons)

    } else {
      val arg: (VariableTerm, ArgumentType) = to_assign.head
      arg._2 match {
        case BoundedIntervalArgument(min, max) => for (i <- min to max) combine_rule_variables(antecedents,to_assign.tail,assigned+(arg._1->NumberTerm(i)),cons)
        case ConstantArgument(term) => combine_rule_variables(antecedents,to_assign.tail,assigned+(arg._1->term),cons)
        case EnumerableArgument(set_name) =>
          val cat = builder.onto.get_category_by_name(set_name)
          for (value <- cat.range)
            combine_rule_variables(antecedents,to_assign.tail,assigned+(arg._1->value),cons)
        case _ => throw new UnsupportedArgumentType
      }
    }
  }

  /**
   * sostituisce tutti i valori al predicato consequent, e crea il p-node corrispondente
   * poi, chiede la generazione dei beta-node o degli alpha-node
   * */
  private def register_pred_nodes(antecedents: List[RuleCondition], assigned: Map[VariableTerm, ConstantTerm], cons : Predicate): Unit = {

    //    val ground_args: List[ConstantTerm] = for (a <- cons.terms) yield a match {
    //      case AnonymousTerm() => throw new UnsupportedArgumentType
    //      case term: VariableTerm => assigned(term)
    //      case term: ConstantTerm => term
    //    }
    //    val p = Proposition(cons.functional,ground_args)


    val p = cons.to_proposition(assigned)
    val production = builder.direct(p)

    val pnode = rete.get_or_create_prod_node(production)

    if (antecedents.size > 1)
      register_beta_nodes(antecedents, assigned, pnode)
    else
      register_alpha_node(antecedents.head,assigned,pnode)

    //note: this last instruction should be replaced by using dummy-beta-nodes
    //i.e. a beta node that has only 1 predecessor and always has a token from the other side
  }

  /**
   * a due a due gli antecedenti generano dei beta node che permettono di derivare il p-node
   * */
  private def register_beta_nodes(antecedents: List[RuleCondition], assigned: Map[VariableTerm, ConstantTerm], consequence : ReteNode) : ReteNode = {
    val beta = rete.get_or_create_beta_node(consequence)

    if (antecedents.size>2) {
      val ante = antecedents.head
      val left = register_alpha_node(ante,assigned,beta)
      val right = register_beta_nodes(antecedents.tail,assigned,beta)
      beta.set_left(left)
      beta.set_right(right)
      beta

    } else {  //antecedents.size==2
      val ante_left = antecedents.head
      val ante_right = antecedents.tail.head
      val left = register_alpha_node(ante_left,assigned,beta)
      val right = register_alpha_node(ante_right,assigned,beta)
      beta.set_left(left)
      beta.set_right(right)
      beta
    }
  }

  def register_alpha_node(antecedent: RuleCondition, assigned: Map[VariableTerm, ConstantTerm], consequence: ReteNode) : ReteNode = {

    antecedent match {
      case expression: Expression => throw new UnsupportedConditionType

      case PredicateCondition(p) =>
        val prop: Proposition = p.to_proposition(assigned)
        val activation = builder.direct(prop)

        val alpha = rete.get_or_create_alpha_node(activation,false)
        alpha.consequences = consequence :: alpha.consequences
        alpha

      case NegateCondition(p) =>
        val prop: Proposition = p.to_proposition(assigned)
        val activation = builder.direct(prop)

        val alpha = rete.get_or_create_alpha_node(activation,true)
        alpha.consequences = consequence :: alpha.consequences
        alpha

      case _ => throw new UnsupportedConditionType
    }

  }

}

class UnsupportedConditionType extends Exception


object RunReteBuilder extends App {
  val domain_parser = new DomainOntologyParser

  val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
    "category users atom [ luca,john,claudia ] " +
    "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
    "category sensor_id number [ 1,2,3 ]  " +

    "define input(enum[users])" +
    "define output(enum[users])" +
    "define room(enum[rooms])" +

    "rule input(?a), not output(?b) => room(\"kitchen\")" +

    "}")

  val onto = onto_parser.get
  println(onto.axioms)

  val logic_builder = new SubLogicBuilder(onto)
  val rete_builder = new RETEBuilder(logic_builder,onto.axioms)

  println(rete_builder.rete.stringGraphviz)


}