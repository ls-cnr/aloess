package org.icar.symbolic

/** An AbstractCapability is a description of a class of actions the system is
 * able to perform in order to achieve some desired results
 * It is identified by an univoque name
 * some parameters and the description of its usage
 * Example:
 * Capability OpenSwitch[switchID : Switcher] {
 * ...
 * }
 *
 * @param id      univoque name of the Capability
 * @param params  list of parameters that characterizes different resulting actions
 * @param pre     description of preconditions in order the action may be seelcted
 * @param post    description of postconditions to be true in the state just after the action execution
 * @param effects description of how the state will be affected by the actions
 * @param future  goals that MUST hold after the action execution
 *
 */
case class AbstractCapability(
                               id: String,
                               params: List[CapabilityParameter],
                               pre: LogicFormula with FOLNature,
                               post: LogicFormula with FOLNature,
                               effects: List[EvolutionGrounding],
                               future: LogicFormula with LTLNature
                             )

case class CapabilityParameter(variable : VariableTerm, category : String)


case class CapabilityParameterEntry(variable : VariableTerm, value : ConstantTerm)
case class CapabilityEntry(cap:AbstractCapability, pars : List[CapabilityParameterEntry])

/** Class used for describing how an AbstractCapability modifies the current state of the world
 * The evolution represents the changes from W(t) to W(t+1) in terms of add/remove predicates
 *
 * @param name an evolution is marked with a name
 * @param evo  specifies the evolution
 */
case class EvolutionGrounding(name: String, evo: List[EvoOperator], probability: Double = 1)

sealed abstract class EvoOperator

/** Add a new predicate into the state
 *
 * @param p predicate to be added
 */
case class AddOperator(p: Predicate) extends EvoOperator

/** Remove a predicate (if it exists) from the state
 *
 * @param p predicate to be removed
 */
case class RmvOperator(p: Predicate) extends EvoOperator
