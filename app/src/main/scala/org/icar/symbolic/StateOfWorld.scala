package org.icar.symbolic

case class StateOfWorld (statements : List[GroundPredicate]) {

	/**
	 * pretty print a state of world as [<list_of_predicates>]
	 * @return
	 */
	override def toString: String = {
		return "["+ statements.mkString(",")+"]"
	}

}
